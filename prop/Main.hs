{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Lambda

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

main :: IO ()
main = quickCheck (\t -> parse (pretty t) == Right t)
