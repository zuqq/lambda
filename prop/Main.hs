{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Lambda.Term
import Lambda.Type

import qualified Lambda.Term.Parser as Term
import qualified Lambda.Term.Pretty as Term
import qualified Lambda.Type.Parser as Type
import qualified Lambda.Type.Pretty as Type

instance Arbitrary Term where
    arbitrary = sized go0
      where
        go0 k
            | k <= 1    = go1 k
            | otherwise = oneof [go1 k, go2 k, go3 k]

        go1 _ = do
            NonNegative n <- arbitrary
            pure (Var n)

        go2 k = do
            t <- go0 (k `div` 2)
            pure (Abs t)

        go3 k = do
            t  <- go0 (k `div` 2)
            t' <- go0 (k `div` 2)
            pure (App t t')

instance Arbitrary Type where
    arbitrary = sized go0
      where
        go0 k
            | k <= 1    = go1 k
            | otherwise = oneof [go1 k, go2 k]

        go1 _ = do
            NonNegative n <- arbitrary
            pure (TypeVar n)

        go2 k = do
            a  <- go0 (k `div` 2)
            a' <- go0 (k `div` 2)
            pure (a :-> a')

main :: IO ()
main = do
    quickCheck (\t -> Term.parse (Term.pretty t) == Right t)
    quickCheck (\t -> Type.parse (Type.pretty t) == Right t)
