module Lambda.Pretty where

import Text.PrettyPrint.HughesPJ (Doc)

class Pretty a where
    ppr :: Int -> a -> Doc

    pp :: a -> Doc
    pp = ppr 0
