module Lambda.Inference where

import Control.Monad.Trans.State.Strict (gets, modify, runState, State)
import qualified Data.IntMap.Strict     as IM
import qualified Data.IntSet            as IS
import Data.Maybe                       (fromMaybe)


-- Terms

data Term = Var Int | Abs Term | App Term Term
    deriving (Eq, Read, Show)

-- Types

data Type = TVar Int | TArr Type Type
    deriving (Eq, Read, Show)

-- | Find the type variables appearing in a type.
tVars :: Type -> IS.IntSet
tVars (TVar n)   = IS.singleton n
tVars (TArr a b) = IS.union (tVars a) (tVars b)

-- Typing context

-- | Mapping from variables to types.
data Context = Context
    { depth :: Int             -- ^ Distance from the outermost scope.
    , table :: IM.IntMap Type  -- ^ Finite mapping from variables to types.
    }
    deriving (Eq, Read, Show)

find :: Int -> Context -> Maybe Type
find n c = IM.lookup (n - depth c) (table c)

-- | Find the largest type variable index that is mentioned in a context.
maxTVar :: Context -> Int
maxTVar c = foldr (max . IS.findMax . tVars) 0 (table c)

-- | Adjust the context when going under an abstraction.
under
    :: Type     -- ^ Type for the bound variable under the lambda.
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
under a c = Context
    { depth = depth c + 1
    , table = IM.insert (-(depth c + 1)) a (table c)
    }

-- Gathering constraints

data Constr = CEq Type Type
    deriving (Eq, Read, Show)

data GatherState = GatherState
    { index :: Int       -- ^ Fresh type variable index.
    , acc   :: [Constr]  -- ^ List of collected constraints.
    }
    deriving (Eq, Read, Show)

type Gather a = State GatherState a

-- | Get a fresh type variable.
freshTVar :: Gather Type
freshTVar = do
    i <- gets index
    modify $ \s -> s { index = i + 1 }
    return $ TVar i

-- | Record a constraint.
record :: Constr -> Gather ()
record constr = modify $ \s -> s { acc = constr : acc s }

-- | Gather the type constraints for the given term. Variables that are not
-- typed by the context get freshed type variables, keeping their types as
-- general as possible.
gather :: Context -> Term -> Gather Type
gather c (Var n) = maybe freshTVar return (find n c)
gather c (Abs t) = do
    x <- freshTVar
    b <- gather (under x c) t
    return $ TArr x b
gather c (App t t') = do
    a  <- gather c t
    a' <- gather c t'
    x  <- freshTVar
    record $ CEq a (TArr a' x)
    return x

runGather :: Context -> Term -> (Type, [Constr])
runGather c t = (a, acc s')
  where
    (a, s') = runState (gather c t) (GatherState (maxTVar c + 1) [])

-- Substitutions

type Sub = IM.IntMap Type

-- | Apply a substitution to a type.
apply :: Sub -> Type -> Type
apply s (TVar n)   = IM.findWithDefault (TVar n) n s
apply s (TArr a b) = TArr (apply s a) (apply s b)

-- | Apply a substitution to both sides of a constraint.
applyBoth :: Sub -> Constr -> Constr
applyBoth s (CEq a b) = CEq (apply s a) (apply s b)

-- | Substitution composition.
--
-- The domain of the substitution @s' `after` s@ is the union of the domains
-- of @s@ and @s'@. It maps a type variable @a@ in its domain as follows:
-- 
--     * If @a@ lies in the domain of @s@, then
--     @
--         apply (s' `after` s) a == apply s' (apply s a)
--     @
--
--     * If @a@ lies in the domain of @s'@ but not in that of @s@, then
--     @
--         apply (s' `after` s) a == apply s'
--     @
after :: Sub -> Sub -> Sub
after s' s = IM.union (IM.map (apply s') s) s'

-- Solving constraints

-- | Failure mode for constraint unification.
data RecException = RecException Type Type

-- | Try to find a substitution that solves the given constraints. The only way
-- for this to fail is if it encounters a constraint of the form x = a for a
-- type variable x and type a with x free in a.
unify :: [Constr] -> Either RecException Sub
unify []        = Right IM.empty
unify ((CEq a b) : rest)
    | a == b    = unify rest
    | otherwise = case (a, b) of
        (TVar m, b)
            | m `IS.member` tVars b -> Left $ RecException a b
            | otherwise             -> do
                let s = IM.singleton m b
                s' <- unify (map (applyBoth s) rest)
                return $ s' `after` s
        (a, TVar n)                 -> unify $ CEq b a : rest
        (TArr a a', TArr b b')      -> unify $ CEq a b : CEq a' b' : rest
