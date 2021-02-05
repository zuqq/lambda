module Lambda.Typed where

import qualified Text.PrettyPrint.HughesPJ as PrettyPrint

import Lambda.Pretty
import Lambda.Type   (Sub, Type, apply)
import qualified Lambda.Type as Type (Type (..))

-- | Typed terms.
data Term
    = Var Int Type
    | Abs Term Type
    | App Term Term Type
    deriving (Eq, Read, Show)

instance Pretty Term where
    ppr _ (Var n a)    = PrettyPrint.parens . PrettyPrint.hsep $
        [ PrettyPrint.text ("v" <> show n)
        , PrettyPrint.char ':'
        , ppr 0 a
        ]
    ppr d (Abs b _)    = PrettyPrint.maybeParens (d > 0) . PrettyPrint.hsep $
        [ PrettyPrint.text "\\v0"
        , PrettyPrint.text "->"
        , ppr 0 b
        ]
    ppr _ (App a a' _) = PrettyPrint.hsep
        [ ppr 1 a
        , ppr 1 a'
        ]

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
