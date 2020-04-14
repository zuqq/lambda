{-# LANGUAGE LambdaCase #-}

module Lambda.Typed (Context, Term (..), Type (..), typeof) where

import qualified Data.IntMap as IM


data Term = Var Int | Abs Type Term | App Term Term
    deriving (Eq, Read, Show)

data Type = TyVar Int | TyArr Type Type
    deriving (Eq, Read, Show)

type Context = IM.IntMap Type

typeof :: Context -> Term -> Maybe Type
typeof c (Var n) = IM.lookup n c
typeof c (Abs a t) = do
    b <- typeof c t
    Just $ TyArr a b
typeof c (App t t') = typeof c t >>= \case
    TyVar _   -> Nothing
    TyArr a b -> do
        a' <- typeof c t'
        if a' == a
            then Just b
            else Nothing
