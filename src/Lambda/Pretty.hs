{-# LANGUAGE OverloadedStrings #-}

module Lambda.Pretty (pretty) where

import Data.Text (Text)
import Prelude   hiding (abs)
import qualified Data.Text as Text (pack)

import Lambda.Untyped

-- $setup
-- >>> :set -XOverloadedStrings

-- | Pretty print a 'Term'.
--
-- ==== __Examples__
--
-- >>> pretty (Var 0)
-- "0"
-- >>> pretty (App (Var 0) (Var 1))
-- "0 1"
-- >>> pretty (Abs (Abs (App (Var 0) (Var 1))))
-- "\\ \\ 0 1"
pretty :: Term -> Text
pretty = abs

abs :: Term -> Text
abs (Abs t) = "\\ " <> abs t
abs t       = app t

app :: Term -> Text
app (App t t') = var t <> " " <> var t'
app t          = var t

var :: Term -> Text
var (Var n) = Text.pack (show n)
var t       = "(" <> abs t <> ")"
