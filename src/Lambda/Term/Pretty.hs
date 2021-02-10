{-# LANGUAGE OverloadedStrings #-}

module Lambda.Term.Pretty (pretty) where

import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Text as Text (pack)

import Lambda.Term.Internal

-- | Print a term.
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
