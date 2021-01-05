module Lambda.Pretty
    ( Pretty (..)
    )
    where

import qualified Text.PrettyPrint.HughesPJ as PP


class Pretty a where
    ppr :: Int -> a -> PP.Doc

    pp :: a -> PP.Doc
    pp = ppr 0
