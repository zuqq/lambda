module Lambda.Typed where

import Lambda.Type (Sub, Type, apply)

-- | Typed terms.
data Term
    = Var Int Type
    | Abs Term Type
    | App Term Term Type
    deriving (Eq, Read, Show)

-- | Extract the type of a 'Term'.
typeof :: Term -> Type
typeof (Var _ t)   = t
typeof (Abs _ t)   = t
typeof (App _ _ t) = t

-- | Apply a substitution to a 'Term'.
sub :: Sub -> Term -> Term
sub s (Var n t)    = Var n (apply s t)
sub s (Abs b t)    = Abs (sub s b) (apply s t)
sub s (App a a' t) = App (sub s a) (sub s a') (apply s t)
