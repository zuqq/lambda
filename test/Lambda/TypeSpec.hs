{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambda.TypeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck

import Lambda.Term (Term (..))
import Lambda.Type.Internal
import Lambda.Type.Parser
import Lambda.Type.Pretty

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

    shrink (TypeVar _) = mempty
    shrink (a :-> a')  = [a, a'] <> [b :-> b' | (b, b') <- shrink (a, a')]

spec :: Spec
spec = do
    describe "normalize" do
        it "relabels the type variables from left to right" do
            property \a ->
                inorder (normalize a) == [i | (i, _) <- zip [0..] (inorder a)]
        it "is idempotent" do
            property \a -> (normalize . normalize) a `shouldBe` normalize a

    describe "apply" do
        it "is a semigroup homomorphism" do
            property \s s' a ->
                apply (s' `after` s) a == (apply s' . apply s) a
        it "is a monoid homomorphism" do
            property \a -> apply mempty a == id a

    describe "infer" do
        let a = TypeVar 0
            b = TypeVar 1
            c = TypeVar 2

        it "infers the type of id" do
            infer (Abs (Var 0)) `shouldBe` Just (a :-> a)
        it "infers the type of fst" do
            infer (Abs (Abs (Var 1))) `shouldBe` Just (a :-> b :-> a)
        it "infers the type of snd" do
            infer (Abs (Abs (Var 0))) `shouldBe` Just (a :-> b :-> b)
        it "infers the type of apply" do
            infer (Abs (Abs (App (Var 1) (Var 0))))
                `shouldBe` Just ((a :-> b) :-> a :-> b)
        it "infers the type of twice" do
            infer (Abs (Abs (App (Var 1) (App (Var 1) (Var 0)))))
                `shouldBe` Just ((a :-> a) :-> a :-> a)
        it "infers the type of thrice" do
            infer (Abs (Abs (App (Var 1) (App (Var 1) (App (Var 1) (Var 0))))))
                `shouldBe` Just ((a :-> a) :-> a :-> a)
        it "infers the type of comp" do
            infer (Abs (Abs (Abs (App (Var 2) (App (Var 1) (Var 0))))))
                `shouldBe` Just ((a :-> b) :-> (c :-> a) :-> (c :-> b))

        it "rejects sa" do
            infer (Abs (App (Var 0) (Var 0))) `shouldBe` Nothing
        it "rejects the Y combinator" do
            let f = Var 1
                x = Var 0
                y = Abs (App (Abs (App f (App x x))) (Abs (App f (App x x))))
            infer y `shouldBe` Nothing

    describe "parse" do
        it "parses `TypeVar`" do
            parse "0" `shouldBe` Right (TypeVar 0)
        it "parses `:->`" do
            parse "0 -> 1" `shouldBe` Right (TypeVar 0 :-> TypeVar 1)
        it "parses nested `:->`" do
            parse "0 -> (1 -> 1)"
                `shouldBe` Right (TypeVar 0 :-> (TypeVar 1 :-> TypeVar 1))
        it "is a left inverse of `pretty`" do
            property \a -> (parse . pretty) a == Right a

    describe "pretty" do
        it "prints `TypeVar`" do
            pretty (TypeVar 0) `shouldBe` "0"
        it "prints `:->`" do
            pretty (TypeVar 0 :-> TypeVar 1) `shouldBe` "0 -> 1"
        it "prints nested `:->`" do
            pretty (TypeVar 0 :-> (TypeVar 1 :-> TypeVar 1))
                `shouldBe` "0 -> (1 -> 1)"
