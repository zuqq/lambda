module Untyped (beta, free, shift, sub, Term (..)) where

import qualified Data.IntSet as IS


data Term = Var Int | Abs Term | App Term Term
    deriving (Eq, Read, Show)

free :: Term -> IS.IntSet
free (Var n)    = IS.singleton n
free (Abs t)    = IS.filter (>= 0) . IS.map (+ (-1)) $ free t
free (App t t') = IS.union (free t) (free t')

-- | Applying @shift i c@ to a term @t@ shifts the free variables of @t@ that
-- are greater than or equal to @c@ by @i@.
shift
    :: Int   -- ^ Amount
    -> Int   -- ^ Cutoff
    -> Term
    -> Term
shift i c (Var n)
    | n >= c         = Var (n + i)
    | otherwise      = Var n
shift i c (Abs e)    = Abs (shift i (c + 1) e)
shift i c (App t t') = App (shift i c t) (shift i c t')

-- | Applying @sub t m@ to a term @t'@ replaces all occurences of @m@ among the
-- free variables of @t'@ by @t@.
sub
    :: Term  -- ^ Term to replace with
    -> Int   -- ^ Variable to replace
    -> Term  -- ^ Object
    -> Term
sub t m (Var n)
    | m == n         = t
    | otherwise      = Var n
sub t m (Abs t')     = Abs (sub (shift 1 0 t) (m + 1) t')
sub t m (App t' t'') = App (sub t m t') (sub t m t'')

beta :: Term -> Term -> Term
beta t t' = shift (-1) 0 (sub (shift 1 0 t') 0 t)

-- | Evaluate a lambda term, using the operational semantics.
eval :: Term -> Term
eval (App (Abs t) (Abs t')) = beta t (Abs t')        -- (E-AppAbs)
eval (App (Abs t) t')       = App (Abs t) (eval t')  -- (E-App2)
eval (App t t')             = App (eval t) t'        -- (E-App1)
eval t                      = t
