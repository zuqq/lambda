module Lambda.Node where

import qualified Text.PrettyPrint.HughesPJ as PP

import Lambda.Pretty
import Lambda.Type


-- | Nodes of the typed AST.
data Node
    = NVar Int Type
    | NAbs Node Type
    | NApp Node Node Type
    deriving (Eq, Read, Show)

instance Pretty Node where
    ppr _ (NVar n a)    = PP.parens . PP.hsep $
        [ PP.text ("v" <> show n)
        , PP.char ':'
        , ppr 0 a
        ]
    ppr d (NAbs b _)    = PP.maybeParens (d > 0) . PP.hsep $
        [ PP.text "\\v0"
        , PP.text "->"
        , ppr 0 b
        ]
    ppr _ (NApp a a' _) = PP.hsep
        [ ppr 1 a
        , ppr 1 a'
        ]

-- | Extract the type of a 'Node'.
typeof :: Node -> Type
typeof (NVar _ t)   = t
typeof (NAbs _ t)   = t
typeof (NApp _ _ t) = t

-- | Apply a substitution to a typed AST.
sub :: Sub -> Node -> Node
sub s (NVar n t)    = NVar n (apply s t)
sub s (NAbs b t)    = NAbs (sub s b) (apply s t)
sub s (NApp a a' t) = NApp (sub s a) (sub s a') (apply s t)
