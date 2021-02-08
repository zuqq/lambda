{-# LANGUAGE OverloadedStrings #-}

module Lambda.Type.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Attoparsec.Text as Attoparsec

import Lambda.Type

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse a 'Type'.
--
-- ==== __Examples__
--
-- >>> parse "0"
-- Right (TypeVar 0)
-- >>> parse "0 -> 1"
-- Right (TypeVar 0 :-> TypeVar 1)
-- >>> parse "0 -> (1 -> 1)"
-- Right (TypeVar 0 :-> (TypeVar 1 :-> TypeVar 1))
parse :: Text -> Either String Type
parse = Attoparsec.parseOnly arr

arr :: Attoparsec.Parser Type
arr = arr0 <|> var
  where
    arr0 = do
        a  <- var
        _  <- " -> "
        a' <- var
        pure (a :-> a')

var :: Attoparsec.Parser Type
var = var0 <|> ("(" *> arr <* ")")
  where
    var0 = do
        n <- Attoparsec.decimal
        pure (TypeVar n)
