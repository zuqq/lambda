module Lambda.Type (Type (..), infer) where

import Control.Monad.Trans.State (State)
import Data.Map (Map)
import Data.Set (Set)
import Lens.Micro (Lens')
import Lens.Micro.Mtl ((+=), modifying, use)

import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Lambda.Term

data Type
    = TypeVar Integer
    | Type :-> Type
    deriving (Eq, Read, Show)

-- | Map a type to the set of its type variables.
free :: Type -> Set Integer
free (TypeVar n) = Set.singleton n
free (a :-> a')  = Set.union (free a) (free a')

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
--
-- NB. @apply@ is a monoid homomorphism:
--
-- > \s s' a -> apply (s' `after` s) a == apply s' (apply s a)
apply :: Substitution -> Type -> Type
apply s (TypeVar n) = Map.findWithDefault (TypeVar n) n s
apply s (a :-> a')  = apply s a :-> apply s a'

-- | Compose two substitutions.
after :: Substitution -> Substitution -> Substitution
after s' s = Map.union (Map.map (apply s') s) s'

-- | A constraint is an equality between two types.
type Constraint = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Substitution -> Constraint -> Constraint
transform s (a, b) = (apply s a, apply s b)

data GatherState
    = GatherState
        Integer       -- ^ Index of the next fresh type variable.
        [Constraint]  -- ^ List of collected constraints.

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
gather :: Context -> Term -> Gather Type
gather ctx (Var n)    = maybe fresh pure (Map.lookup n ctx)
gather ctx (Abs t)    = do
    a  <- fresh
    a' <- gather (bind a ctx) t
    pure (a :-> a')
gather ctx (App t t') = do
    a  <- gather ctx t
    a' <- gather ctx t'
    b  <- fresh
    record (a, a' :-> b)
    pure b

-- | Try to find a substitution that solves the given constraints.
unify :: [Constraint] -> Maybe Substitution
unify []                          = Just Map.empty
unify ((a, b) : cs)
    | a == b                      = unify cs
unify ((TypeVar n, b) : cs)
    | n `Set.member` free b       = Nothing
    | otherwise                   = do
        let s = Map.singleton n b
        s' <- unify (fmap (transform s) cs)
        Just (s' `after` s)
unify ((a, b@(TypeVar _)) : cs)   = unify ((b, a) : cs)
unify ((a :-> a', b :-> b') : cs) = unify ((a, b) : (a', b') : cs)

-- | Try to infer the type of the given term.
infer :: Context -> Term -> Maybe Type
infer ctx t = do
    let i = maybe 0 (+ 1) (Set.lookupMax (foldMap free ctx))
    let (a, GatherState _ cs) = State.runState (gather ctx t) (GatherState i [])
    s <- unify cs
    pure (apply s a)
