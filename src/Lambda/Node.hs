module Lambda.Node where

import qualified Text.PrettyPrint as PP

import Lambda.Type


-- | Nodes of the typed AST.
data Node
    = NVar Int Type
    | NAbs Node Type
    | NApp Node Node Type
    deriving (Eq, Read, Show)

-- | Pretty-printing for 'Node's.
ppN :: Int -> Node -> PP.Doc
ppN d (NVar n a)    = PP.parens . PP.hsep $
    [ PP.text ("v" <> show n)
    , PP.char ':'
    , ppT 0 a
    ]
ppN d (NAbs b t)    = parens d . PP.hsep $
    [ PP.text "\\v0"
    , PP.text "->"
    , ppN 0 b
    ]
  where
    parens 0 = id
    parens _ = PP.parens
ppN d (NApp a a' t) = PP.hsep
    [ ppN 1 a
    , ppN 1 a'
    ]

-- | Extract the type of a 'Node'.
typeof :: Node -> Type
typeof (NVar _ t)    = t
typeof (NAbs _ t)    = t
typeof (NApp _ _ t ) = t

-- | Apply a substitution to a typed AST.
sub :: Sub -> Node -> Node
sub s (NVar n t)    = NVar n (apply s t)
sub s (NAbs b t)    = NAbs (sub s b) (apply s t)
sub s (NApp a a' t) = NApp (sub s a) (sub s a') (apply s t)
