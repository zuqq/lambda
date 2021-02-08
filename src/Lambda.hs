{-# LANGUAGE OverloadedStrings #-}

module Lambda (infer, parse, pretty) where

import Data.Text (Text)

import Lambda.Term
import Lambda.Term.Parser
import Lambda.Type

import qualified Lambda.Term.Pretty as Term
import qualified Lambda.Type.Pretty as Type

-- | Pretty print a typed term.
pretty :: (Term, Type) -> Text
pretty (t, a) = Term.pretty t <> " : " <> Type.pretty a
