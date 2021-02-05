module Lambda.Type where

import qualified Text.PrettyPrint.HughesPJ as PP

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Lambda.Pretty

-- | A type is either a type variable or a function type. There is an infinite
-- number of type variables, indexed by the natural numbers.
data Type = TVar Int | TArr Type Type
    deriving (Eq, Read, Show)

instance Pretty Type where
    ppr _ (TVar n)    = PP.text $ "a" <> show n
    ppr d (TArr a a') = PP.maybeParens (d > 0) . PP.hsep $
        [ ppr (d + 1) a
        , PP.text "->"
        , ppr (d + 1) a'
        ]

-- | Map a type to the set of its type variables.
free :: Type -> S.Set Int
free (TVar n)   = S.singleton n
free (TArr a b) = S.union (free a) (free b)

-- | A typing context is a finite mapping from free variables to types.
type Context = M.Map Int Type

-- | Adjust the context when going under an abstraction.
bind
    :: Type     -- ^ Type for the bound variable of the abstraction
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
bind a = M.insert 0 a . M.mapKeys (+ 1)

-- | A substitution is a finite mapping from type variable indices to types.
type Sub = M.Map Int Type

-- | Apply a substitution to a type.
apply :: Sub -> Type -> Type
apply s (TVar n)   = M.findWithDefault (TVar n) n s
apply s (TArr a b) = TArr (apply s a) (apply s b)

-- | Compose two substitutions. The substitution @s' `after` s@ satisfies
-- @
-- apply (s' `after` s) a = s' `apply` (s `apply` a)
-- @
-- for every @a@.
after :: Sub -> Sub -> Sub
after s' s = M.union (M.map (apply s') s) s'
