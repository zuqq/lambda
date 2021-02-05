module Lambda.Untyped where

data Term = Var Int | Abs Term | App Term Term
    deriving (Eq, Read, Show)

shift
    :: Int   -- ^ Amount
    -> Int   -- ^ Cutoff
    -> Term
    -> Term
shift i c (Var n)
    | n >= c         = Var (n + i)
    | otherwise      = Var n
shift i c (Abs t)    = Abs (shift i (c + 1) t)
shift i c (App t t') = App (shift i c t) (shift i c t')

-- | @sub t m t'@ is @t'@ with all occurences of @Var m@ replaced by @t@.
sub
    :: Term  -- ^ Term to replace with
    -> Int   -- ^ Index of the variable to replace
    -> Term  -- ^ Object
    -> Term
sub t m (Var n)
    | m == n         = t
    | otherwise      = Var n
sub t m (Abs t')     = Abs (sub (shift 1 0 t) (m + 1) t')
sub t m (App t' t'') = App (sub t m t') (sub t m t'')

beta :: Term -> Term -> Term
beta t t' = shift (-1) 0 (sub (shift 1 0 t') 0 t)

-- | Evaluate a 'Term', using a call-by-value strategy.
eval :: Term -> Term
eval (App (Abs t) (Abs t')) = beta t (Abs t')        -- (E-AppAbs)
eval (App (Abs t) t')       = App (Abs t) (eval t')  -- (E-App2)
eval (App t t')             = App (eval t) t'        -- (E-App1)
eval t                      = t
