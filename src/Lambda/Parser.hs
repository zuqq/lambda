{-# LANGUAGE OverloadedStrings #-}

module Lambda.Parser (parse) where

import Control.Applicative ((<|>))
import Data.Text           (Text)
import Lambda.Untyped
import Prelude             hiding (abs)

import qualified Data.Attoparsec.Text as Attoparsec

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse a 'Term'.
--
-- ==== __Examples__
--
-- >>> parse "0"
-- Right (Var 0)
-- >>> parse "0 1"
-- Right (App (Var 0) (Var 1))
-- >>> parse "\\ \\ 0 1"
-- Right (Abs (Abs (App (Var 0) (Var 1))))
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
