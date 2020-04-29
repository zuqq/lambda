module Lambda.Inference where

import Control.Monad.Trans.State.Strict (gets, modify, runState, State)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S

import Lambda.Term
import Lambda.Type


-- Gathering constraints

-- | A constraint is an equality between two types.
type Constr = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Sub -> Constr -> Constr
transform s (a, b) = (apply s a, apply s b)

data GatherState = GatherState
    { index :: Int       -- ^ Fresh type variable index.
    , acc   :: [Constr]  -- ^ List of collected constraints.
    }
    deriving (Eq, Read, Show)

type Gather a = State GatherState a

-- | Get a fresh type variable.
freshTV :: Gather Type
freshTV = do
    i <- gets index
    modify $ \s -> s { index = i + 1 }
    return $ TVar i

-- | Record a constraint.
record :: Constr -> Gather ()
record constr = modify $ \s -> s { acc = constr : acc s }

-- | Gather the type constraints for the given term. Variables that are not
-- typed by the context get fresh type variables, keeping their types as
-- general as possible.
gather :: Context -> Term -> Gather Type
gather c (Var n) = maybe freshTV return (M.lookup n c)
gather c (Abs t) = do
    x <- freshTV
    b <- gather (bind x c) t
    return $ TArr x b
gather c (App t t') = do
    a  <- gather c t
    a' <- gather c t'
    x  <- freshTV
    record (a, TArr a' x)
    return x

runGather :: Context -> Term -> (Type, [Constr])
runGather c t = (a, acc s')
  where
    allTVar = foldr (S.union . free) S.empty c
    newTVar = if S.null allTVar then 0 else S.findMax allTVar + 1
    (a, s') = runState (gather c t) (GatherState newTVar [])

-- Solving constraints

-- | Try to find a substitution that solves the given constraints.
unify :: [Constr] -> Maybe Sub
unify []              = Just M.empty
unify ((a, b) : rest) = if a == b
    then unify rest
    else case (a, b) of
        (TVar m, b)
            | m `S.member` free b -> Nothing
            | otherwise           -> let s = M.singleton m b
                in (`after` s) <$> unify (map (transform s) rest)
        (a, TVar n)               -> unify $ (b, a) : rest
        (TArr a a', TArr b b')    -> unify $ (a, b) : (a', b') : rest
