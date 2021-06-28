{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.TermSpec (spec) where

import Prelude hiding (id)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck

import Lambda.Term.Internal
import Lambda.Term.Parser
import Lambda.Term.Pretty

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

    shrink (Var _)    = mempty
    shrink (Abs t)    = t : [Abs u | u <- shrink t]
    shrink (App t t') = [t, t'] <> [App u u' | (u, u') <- shrink (t, t')]

spec :: Spec
spec = do
    describe "eval" do
        it "conforms to Pierce's example" do
            let id = Abs (Var 0)
            eval (App id (App id (Abs (App id (Var 0)))))
                `shouldBe` Abs (App id (Var 0))

    describe "parse" do
        it "parses `Var`" do
            parse "0" `shouldBe` Right (Var 0)
        it "parses `App`" do
            parse "0 1" `shouldBe` Right (App (Var 0) (Var 1))
        it "parses `Abs`" do
            parse "\\ \\ 0 1" `shouldBe` Right (Abs (Abs (App (Var 0) (Var 1))))
        it "parses the Y combinator" do
            let f = Var 1
                x = Var 0
                y = Abs (App (Abs (App f (App x x))) (Abs (App f (App x x))))
            parse "\\ (\\ 1 (0 0)) (\\ 1 (0 0))" `shouldBe` Right y
        it "is a left inverse of `pretty`" do
            property \t -> (parse . pretty) t == Right t

    describe "pretty" do
        it "prints `Var`" do
            pretty (Var 0) `shouldBe` "0"
        it "prints `App`" do
            pretty (App (Var 0) (Var 1)) `shouldBe` "0 1"
        it "prints `Abs`" do
            pretty (Abs (Abs (App (Var 0) (Var 1)))) `shouldBe` "\\ \\ 0 1"
