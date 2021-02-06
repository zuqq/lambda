module Lambda.Type
    (
    -- * Type
      Type (..)
    , free
    -- * Context
    , Context
    , bind
    -- * Sub
    , Sub
    , after
    , apply
    )
    where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A type is either a type variable or an arrow type.
--
-- There is an infinite number of type variables, indexed by the integers.
data Type = Var Int | Arr Type Type
    deriving (Eq, Read, Show)

-- | Map a type to the set of its type variables.
free :: Type -> Set Int
free (Var n)   = Set.singleton n
free (Arr a b) = Set.union (free a) (free b)

-- | A typing context is a finite mapping from free variables to types.
type Context = Map Int Type

-- | Adjust the context when going under an abstraction.
bind
    :: Type     -- ^ Type for the bound variable of the abstraction
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
bind a = Map.insert 0 a . Map.mapKeys (+ 1)

-- | A substitution is a finite mapping from type variable indices to types.
type Sub = Map Int Type

-- | Apply a substitution to a type.
apply :: Sub -> Type -> Type
apply s (Var n)   = Map.findWithDefault (Var n) n s
apply s (Arr a b) = Arr (apply s a) (apply s b)

-- | Compose two substitutions.
--
-- The substitution @s' `after` s@ satisfies
--
-- > apply (s' `after` s) a = s' `apply` (s `apply` a)
--
-- for every @a@.
after :: Sub -> Sub -> Sub
after s' s = Map.union (Map.map (apply s') s) s'
