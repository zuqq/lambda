{-# LANGUAGE OverloadedStrings #-}

module Lambda.Type.Pretty (pretty) where

import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Text as Text (pack)

import Lambda.Type.Internal

-- | Print a type.
pretty :: Type -> Text
pretty = arr

arr :: Type -> Text
arr (a :-> a') = var a <> " -> " <> var a'
arr a          = var a

var :: Type -> Text
var (TypeVar n) = Text.pack (show n)
var a           = "(" <> arr a <> ")"
