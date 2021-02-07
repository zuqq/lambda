module Lambda.Untyped (Term (..), eval) where

data Term
    = Var Integer
    | Abs Term
    | App Term Term
    deriving (Eq, Read, Show)

-- | @shift i c@ adds @i@ to all variable indices greater than or equal to @c@.
shift :: Integer -> Integer -> Term -> Term
shift i c (Var n)
    | n >= c         = Var (n + i)
    | otherwise      = Var n
shift i c (Abs t)    = Abs (shift i (c + 1) t)
shift i c (App t t') = App (shift i c t) (shift i c t')

-- | @subst t m@ replaces all occurences of @Var m@ by @t@.
subst :: Term -> Integer -> Term -> Term
subst t m (Var n)
    | m == n           = t
    | otherwise        = Var n
subst t m (Abs t')     = Abs (subst (shift 1 0 t) (m + 1) t')
subst t m (App t' t'') = App (subst t m t') (subst t m t'')

beta :: Term -> Term -> Term
beta t t' = shift (-1) 0 (subst (shift 1 0 t') 0 t)

-- | Evaluate a 'Term', using a call-by-value strategy.
eval :: Term -> Term
eval (App (Abs t) (Abs t')) = beta t (Abs t')        -- (E-AppAbs)
eval (App (Abs t) t')       = App (Abs t) (eval t')  -- (E-App2)
eval (App t t')             = App (eval t) t'        -- (E-App1)
eval t                      = t
