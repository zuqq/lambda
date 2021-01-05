module Lambda.Infer where

import Control.Monad.Trans.State.Strict (gets, modify, runState, State)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S

import Lambda.Node
import Lambda.Term
import Lambda.Type

-- | A constraint is an equality between two types.
type Constr = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Sub -> Constr -> Constr
transform s (a, b) = (apply s a, apply s b)

data GatherState = GatherState
    { ix  :: Int       -- ^ Fresh type variable index.
    , acc :: [Constr]  -- ^ List of collected constraints.
    }
    deriving (Eq, Read, Show)

type Gather a = State GatherState a

-- | Get a fresh type variable.
fresh :: Gather Type
fresh = do
    i <- gets ix
    modify $ \s -> s {ix = i + 1}
    pure $ TVar i

-- | Record a constraint.
record :: Constr -> Gather ()
record c = modify $ \s -> s {acc = c : acc s}

-- | Gather the type constraints for the given term. Variables that are not
-- typed by the context get fresh type variables, keeping their types as
-- general as possible.
gather :: Context -> Term -> Gather Node
gather g (Var n) = case M.lookup n g of
    Nothing -> NVar n <$> fresh
    Just a  -> pure $ NVar n a
gather g (Abs t) = do
    x <- fresh
    b <- gather (bind x g) t
    pure $ NAbs b (TArr x (typeof b))
gather g (App t t') = do
    a  <- gather g t
    a' <- gather g t'
    x  <- fresh
    record (typeof a, TArr (typeof a') x)
    pure $ NApp a a' x

runGather :: Context -> Term -> (Node, [Constr])
runGather g t = (a, acc s')
  where
    allTVar = foldr (S.union . free) S.empty g
    newTVar = if S.null allTVar then 0 else S.findMax allTVar + 1
    (a, s') = runState (gather g t) (GatherState newTVar [])

-- | Try to find a substitution that solves the given constraints.
unify :: [Constr] -> Maybe Sub
unify []              = Just M.empty
unify ((a, b) : rest) = if a == b
    then unify rest
    else case (a, b) of
        (TVar m, _)
            | m `S.member` free b -> Nothing
            | otherwise           -> let s = M.singleton m b
                in (`after` s) <$> unify (map (transform s) rest)
        (_, TVar _)               -> unify $ (b, a) : rest
        (TArr c c', TArr d d')    -> unify $ (c, d) : (c', d') : rest

-- | Try to infer the type of the given term.
--
-- The return value is the typed AST, with the subsitution returned by 'unify'
-- already applied.
infer :: Context -> Term -> Maybe Node
infer g t = do
    s <- unify cs
    pure $ sub s n
  where
    (n, cs) = runGather g t
