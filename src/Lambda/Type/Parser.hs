{-# LANGUAGE OverloadedStrings #-}

module Lambda.Type.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Attoparsec.Text as Attoparsec

import Lambda.Type.Internal

-- | Parse a 'Type'.
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
