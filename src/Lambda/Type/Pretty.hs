{-# LANGUAGE OverloadedStrings #-}

module Lambda.Type.Pretty (pretty) where

import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Text as Text (pack)

import Lambda.Type

-- $setup
-- >>> :set -XOverloadedStrings

-- | Pretty print a 'Term'.
--
-- ==== __Examples__
--
-- >>> pretty (TypeVar 0)
-- "0"
-- >>> pretty (TypeVar 0 :-> TypeVar 1)
-- "0 -> 1"
-- >>> pretty (TypeVar 0 :-> (TypeVar 1 :-> TypeVar 1))
-- "0 -> (1 -> 1)"
pretty :: Type -> Text
pretty = arr

arr :: Type -> Text
arr (a :-> a') = var a <> " -> " <> var a'
arr a          = var a

var :: Type -> Text
var (TypeVar n) = Text.pack (show n)
var a           = "(" <> arr a <> ")"
