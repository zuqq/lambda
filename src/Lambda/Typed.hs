module Lambda.Typed (Term (..), Type (..), infer) where

import Control.Monad.Trans.State (State)
import Data.Map                  (Map)
import Data.Set                  (Set)
import Lens.Micro                (Lens')
import Lens.Micro.Mtl            ((+=), modifying, use, view)

import qualified Control.Monad.Trans.State as State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Lambda.Untyped            as Untyped

data Type
    = TypeVar Integer
    -- ^ Infinite store of type variables.
    | Type :-> Type
    -- ^ Arrow types.
    deriving (Eq, Read, Show)

-- | Map a type to the set of its type variables.
free :: Type -> Set Integer
free (TypeVar n)  = Set.singleton n
free (a :-> b)    = Set.union (free a) (free b)

data Term
    = Var Integer Type
    | Abs Term Type
    | App Term Term Type
    deriving (Eq, Read, Show)

-- | Extract the type of a term.
typeof :: Term -> Type
typeof (Var _ a)   = a
typeof (Abs _ a)   = a
typeof (App _ _ a) = a

-- | A typing context is a finite mapping from free variables to types.
type Context = Map Integer Type

-- | Adjust the context when going under an abstraction.
bind
    :: Type     -- ^ Type for the bound variable of the abstraction.
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
bind a = Map.insert 0 a . Map.mapKeys (+ 1)

-- | A substitution is a finite mapping from type variable indices to types.
type Substitution = Map Integer Type

-- | Apply a substitution to a type.
apply :: Substitution -> Type -> Type
apply s (TypeVar n) = Map.findWithDefault (TypeVar n) n s
apply s (a :-> b)   = apply s a :-> apply s b

-- | Compose two substitutions.
--
-- The substitution @s' `after` s@ satisfies
--
-- > apply (s' `after` s) a = s' `apply` (s `apply` a)
--
-- for every @a@.
after :: Substitution -> Substitution -> Substitution
after s' s = Map.union (Map.map (apply s') s) s'

-- | A constraint is an equality between two types.
type Constraint = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Substitution -> Constraint -> Constraint
transform s (a, b) = (apply s a, apply s b)

data GatherState = GatherState
    Integer
    -- ^ Index of the next fresh type variable.
    [Constraint]
    -- ^ List of collected constraints.
    deriving (Eq, Read, Show)

index :: Lens' GatherState Integer
index p (GatherState i cs) = fmap (`GatherState` cs) (p i)

collected :: Lens' GatherState [Constraint]
collected q (GatherState i cs) = fmap (i `GatherState`) (q cs)

type Gather a = State GatherState a

-- | Get a fresh type variable.
fresh :: Gather Type
fresh = do
    i <- use index
    index += 1
    pure (TypeVar i)

-- | Record a constraint.
record :: Constraint -> Gather ()
record c = modifying collected (c :)

-- | Gather the type constraints for the given term.
--
-- Variables that are not typed by the context get fresh type variables,
-- keeping their types as general as possible.
gather :: Context -> Untyped.Term -> Gather Term
gather c (Untyped.Var n)    = case Map.lookup n c of
    Nothing -> fmap (Var n) fresh
    Just a  -> pure (Var n a)
gather c (Untyped.Abs t)    = do
    x <- fresh
    b <- gather (bind x c) t
    pure (Abs b (x :-> typeof b))
gather c (Untyped.App t t') = do
    a  <- gather c t
    a' <- gather c t'
    x  <- fresh
    record (typeof a, typeof a' :-> x)
    pure (App a a' x)

runGather :: Context -> Untyped.Term -> (Term, [Constraint])
runGather c t = (a, view collected s')
  where
    bound   = foldr (Set.union . free) Set.empty c
    next    = maybe 0 (+ 1) (Set.lookupMax bound)
    (a, s') = State.runState (gather c t) (GatherState next [])

-- | Try to find a substitution that solves the given constraints.
unify :: [Constraint] -> Maybe Substitution
unify []              = Just Map.empty
unify ((a, b) : rest) = if a == b
    then unify rest
    else case (a, b) of
        (TypeVar m, _)
            | m `Set.member` free b -> Nothing
            | otherwise             -> do
                let s = Map.singleton m b
                s' <- unify (fmap (transform s) rest)
                Just (s' `after` s)
        (_, TypeVar _)              -> unify ((b, a) : rest)
        (c :-> c', d :-> d')        -> unify ((c, d) : (c', d') : rest)

-- | Try to infer the type of the given term.
--
-- The return value is a typed term, with the substitution returned by 'unify'
-- already applied.
infer :: Context -> Untyped.Term -> Maybe Term
infer g u = do
    let (n, cs) = runGather g u
    s <- unify cs
    pure (go s n)
  where
    go s (Var n t)    = Var n (apply s t)
    go s (Abs b t)    = Abs (go s b) (apply s t)
    go s (App a a' t) = App (go s a) (go s a') (apply s t)
