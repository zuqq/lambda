{-# LANGUAGE OverloadedStrings #-}

module Lambda.Term.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Prelude hiding (abs)

import qualified Data.Attoparsec.Text as Attoparsec

import Lambda.Term.Internal

-- | Parse a 'Term'.
parse :: Text -> Either String Term
parse = Attoparsec.parseOnly abs

abs :: Attoparsec.Parser Term
abs = abs0 <|> app
  where
    abs0 = do
        _ <- "\\ "
        t <- abs
        pure (Abs t)

app :: Attoparsec.Parser Term
app = app0 <|> var
  where
    app0 = do
        t  <- var
        _  <- " "
        t' <- var
        pure (App t t')

var :: Attoparsec.Parser Term
var = var0 <|> ("(" *> abs <* ")")
  where
    var0 = do
        n <- Attoparsec.decimal
        pure (Var n)
