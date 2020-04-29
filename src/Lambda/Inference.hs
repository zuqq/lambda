{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lambda.Inference where

import Control.Monad.Trans.State.Strict (gets, modify, runState, State)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S


-- Terms

newtype VIx = VIx Int
    deriving (Eq, Num, Ord, Read, Show)

-- | Terms are those of the untyped lambda calculus.
data Term = Var VIx | Abs Term | App Term Term
    deriving (Eq, Read, Show)

-- Types

newtype TVIx = TVIx Int
    deriving (Eq, Num, Ord, Read, Show)

-- | A type is either a type variable or a function type. There is an infinite
-- number of type variables, indexed by the natural numbers.
data Type = TVar TVIx | TArr Type Type
    deriving (Eq, Read, Show)

-- | Map a type to the set of its type variables.
tVars :: Type -> S.Set TVIx
tVars (TVar n)   = S.singleton n
tVars (TArr a b) = S.union (tVars a) (tVars b)

-- Typing context

-- | A typing context is a finite mapping from free variables to types.
type Context = M.Map VIx Type

-- | Find the largest type variable index that is mentioned in a context.
maxTVar :: Context -> TVIx
maxTVar = foldr (max . S.findMax . tVars) 0

-- | Adjust the context when going under an abstraction.
bind
    :: Type     -- ^ Type for the bound variable of the abstraction
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
bind a = M.insert 0 a . M.mapKeys (+ 1)

-- Gathering constraints

-- | A constraint is an equality between two types.
data Constr = CEq Type Type
    deriving (Eq, Read, Show)

data GatherState = GatherState
    { index :: TVIx      -- ^ Fresh type variable index.
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
-- typed by the context get fresh type variables, keeping their types as
-- general as possible.
gather :: Context -> Term -> Gather Type
gather c (Var n) = maybe freshTVar return (M.lookup n c)
gather c (Abs t) = do
    x <- freshTVar
    b <- gather (bind x c) t
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

type Sub = M.Map TVIx Type

-- | Apply a substitution to a type.
apply :: Sub -> Type -> Type
apply s (TVar n)   = M.findWithDefault (TVar n) n s
apply s (TArr a b) = TArr (apply s a) (apply s b)

-- | Apply a substitution to both sides of a constraint.
applyBoth :: Sub -> Constr -> Constr
applyBoth s (CEq a b) = CEq (apply s a) (apply s b)

-- | Compose two substitutions. The substitution @s' `after` s@ satisfies
-- @
-- apply (s' `after` s) a = s' `apply` (s `apply` a)
-- @
-- for every @a@.
after :: Sub -> Sub -> Sub
after s' s = M.union (M.map (apply s') s) s'

-- Solving constraints

data RecException = RecException Type Type

-- | Try to find a substitution that solves the given constraints. The only way
-- for this to fail is if it encounters a recursive constraint.
unify :: [Constr] -> Either RecException Sub
unify []        = Right M.empty
unify ((CEq a b) : rest)
    | a == b    = unify rest
    | otherwise = case (a, b) of
        (TVar m, b)
            | m `S.member` tVars b -> Left $ RecException a b
            | otherwise             -> do
                let s = M.singleton m b
                s' <- unify (map (applyBoth s) rest)
                return $ s' `after` s
        (a, TVar n)                 -> unify $ CEq b a : rest
        (TArr a a', TArr b b')      -> unify $ CEq a b : CEq a' b' : rest
