module Lambda.Type.Internal where

import Control.Monad.Trans.RWS.Strict (RWS)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Monoid (Endo (..))
import Data.Sequence (Seq)
import Data.Set (Set)

import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Lambda.Term

data Type
    = TypeVar Integer
    | Type :-> Type
    deriving (Eq, Read, Show)

-- | Normalize a type by relabeling its variables in order.
normalize :: Type -> Type
normalize b = apply (process (inorder b)) b
  where
    inorder :: Type -> Seq Integer
    inorder (TypeVar n) = Seq.singleton n
    inorder (a :-> a')  = inorder a <> inorder a'

    process :: Seq Integer -> Substitution
    process = snd . foldl' step (0, mempty)
      where
        step :: (Integer, Substitution) -> Integer -> (Integer, Substitution)
        step z@(i, m) x
            | x `Map.member` m = z
            | otherwise        =
                i `seq` m `seq` (i + 1, Map.insert x (TypeVar i) m)

-- | Map a type to the set of its type variables.
free :: Type -> Set Integer
free (TypeVar n) = Set.singleton n
free (a :-> a')  = free a <> free a'

-- | A typing context is a finite mapping from type variable indices to types.
type Context = Map Integer Type

-- | Adjust the context when going under an abstraction.
bind :: Type -> Context -> Context
bind a = Map.insert 0 a . Map.mapKeys (+ 1)

-- | A substitution is a finite mapping from type variable indices to types.
type Substitution = Map Integer Type

-- | Apply a substitution to a type.
apply :: Substitution -> Type -> Type
apply s (TypeVar n) = Map.findWithDefault (TypeVar n) n s
apply s (a :-> a')  = apply s a :-> apply s a'

-- | Compose two substitutions.
after :: Substitution -> Substitution -> Substitution
after s' s = Map.map (apply s') s <> s'

-- | A constraint is an equality between two types.
type Constraint = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Substitution -> Constraint -> Constraint
transform s (a, b) = (apply s a, apply s b)

type Gather = RWS Context (Endo [Constraint]) Integer

-- | Get a fresh type variable.
fresh :: Gather Type
fresh = do
    i <- RWS.get
    RWS.put (i + 1)
    pure (TypeVar i)

-- | Gather the constraints for the given term.
gather :: Term -> Gather Type
gather (Var n)    = do
    a <- RWS.asks (Map.lookup n)
    maybe fresh pure a
gather (Abs t)    = do
    a  <- fresh
    a' <- RWS.local (bind a) (gather t)
    pure (a :-> a')
gather (App t t') = do
    a  <- gather t
    a' <- gather t'
    b  <- fresh
    RWS.tell (Endo ((a, a' :-> b) :))
    pure b

-- | Try to find a substitution that solves the given constraints.
unify :: [Constraint] -> Maybe Substitution
unify []                          = Just mempty
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
    let (a, Endo e) = RWS.evalRWS (gather t) ctx i
    s <- unify (e [])
    pure (normalize (apply s a))
  where
    i = maybe 0 (+ 1) (Set.lookupMax (foldMap free ctx))
