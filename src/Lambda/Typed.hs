{-# LANGUAGE LambdaCase #-}

module Lambda.Typed (Context, Term (..), Type (..), typeof) where


data Term = Var Int | Abs Type Term | App Term Term
    deriving (Eq, Read, Show)

data Type = TBase | TArr Type Type
    deriving (Eq, Read, Show)

type Context = [(Int, Type)]

typeof :: Context -> Term -> Maybe Type
typeof c (Var n) = lookup n c
typeof c (Abs a t) = do
    b <- typeof c t
    Just $ TArr a b
typeof c (App t t') = typeof c t >>= \case
    TBase    -> Nothing
    TArr a b -> do
        a' <- typeof c t'
        if a' == a
            then Just b
            else Nothing
