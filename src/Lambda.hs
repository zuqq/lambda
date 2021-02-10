{-# LANGUAGE OverloadedStrings #-}

module Lambda (Type.infer, Term.parse, pretty) where

import Data.Text (Text)

import Lambda.Term (Term)
import Lambda.Type (Type)

import qualified Lambda.Term as Term
import qualified Lambda.Type as Type

-- | Print a typed term.
pretty :: (Term, Type) -> Text
pretty (t, a) = Term.pretty t <> " : " <> Type.pretty a
