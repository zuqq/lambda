module Lambda.Type.Internal where

import Control.Monad.Trans.RWS.Strict (RWS)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Monoid (Endo (..))
import Data.Set (Set)

import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Lambda.Term

infixr 0 :->

data Type
    = TypeVar Integer
    | Type :-> Type
    deriving (Eq, Read, Show)

-- | Map a type to the set of its type variables.
free :: Type -> Set Integer
free (TypeVar n) = Set.singleton n
free (a :-> a')  = free a <> free a'

-- | Left-to-right iteration of the type variables, with duplicates removed.
inorder :: Type -> [Integer]
inorder = nubOrd . toList . go
  where
    go (TypeVar n) = Seq.singleton n
    go (a :-> a')  = go a <> go a'

-- | A typing context is a finite mapping from variable indices to types.
type Context = Map Integer Type

-- | Adjust the context when going under an abstraction.
bind :: Type -> Context -> Context
bind a = Map.insert 0 a . Map.mapKeys (+ 1)

-- | Relabel the type variables from left to right.
normalize :: Type -> Type
normalize a = apply s a
  where
    s = Map.fromList [(n, TypeVar i) | (n, i) <- zip (inorder a) [0..]]

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
infer :: Term -> Maybe Type
infer t = do
    let (a, e) = RWS.evalRWS (gather t) mempty 0
    s <- unify (appEndo e mempty)
    pure (apply s a)
