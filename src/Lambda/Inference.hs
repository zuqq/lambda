{-# LANGUAGE RecordWildCards #-}

module Lambda.Typed where

import Prelude hiding (lookup)

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

free :: Type -> IS.IntSet
free (TVar n)   = IS.singleton n
free (TArr a b) = IS.union (free a) (free b)

-- Typing context

-- | Mapping from variables to types.
data Context = Context
    { depth :: Int             -- ^ Distance from the outermost scope.
    , table :: IM.IntMap Type  -- ^ Finite mapping from variables to types.
    }
    deriving (Eq, Show, Read)

lookup :: Int -> Context -> Maybe Type
lookup n Context {..} = IM.lookup (n - depth) table

-- | Find the smallest non-negative type variable index that is not mentioned
-- in the given context.
freshIndex :: Context -> Int
freshIndex c = max maxKey maxFree + 1
  where
    maxKey = case IM.lookupMax (table c) of
        Nothing     -> -1
        Just (n, _) -> n
    maxFree = foldr (max . IS.findMax . free) (-1) (table c)

-- | Adjust the context when going under an abstraction.
under
    :: Type     -- ^ Type for the bound variable under the lambda.
    -> Context  -- ^ Context for the abstraction.
    -> Context  -- ^ Context for the body of the abstraction.
under t Context {..} = Context
    { depth = depth + 1
    , table = IM.insert (-(depth + 1)) t table
    }

-- Gathering constraints

-- | Type equality constraint.
data Constr = CEq Type Type
    deriving (Eq, Read, Show)

data GatherState = GatherState
    { index :: Int       -- ^ Fresh type variable index.
    , acc   :: [Constr]  -- ^ List of collected constraints.
    }
    deriving (Eq, Read, Show)

type Gather a = State GatherState a

-- | Increment the fresh type variable index.
incrIndex :: Gather ()
incrIndex = modify $ \s -> s { index = index s + 1 }

-- | Record the constraint.
record :: Constr -> Gather ()
record constr = modify $ \s -> s { acc = constr : acc s }

-- | Gather the type constraints for the given term.
--
-- The inference rules are roughly as follows:
--
--     * If the term is a variable, we look it up in the context; if it's not in
--     the context, return a fresh type variable.
--
--     * If the term is an abstraction @Abs t@, we recurse on @t@. Because we
--     are using De Bruijn indices, the context { v_i : T_i } for @Abs t@
--     translates to the context { v_0 : x, x_{i + 1} : T_i } for @t@, where v_0
--     is the bound variable and x is a fresh type variable. Return
--     x -> (infered type of body).
--
--     * If the term is an application @App t t'@, first gather constraints
--     from both terms. Let x be a fresh type variable. Record the constraint
--     (infered type of t) = (infered type of t') -> x and return x.
gather :: Context -> Term -> Gather Type
gather c (Var n) = do
    case lookup n c of
        Just a  -> return a
        Nothing -> do
            i <- gets index
            incrIndex
            return (TVar i)
gather c (Abs t) = do
    i <- gets index
    incrIndex
    let x = TVar i
    b <- gather (under x c) t
    return $ TArr (TVar i) b
gather c (App t t') = do
    a  <- gather c t
    a' <- gather c t'
    i  <- gets index
    incrIndex
    let x = TVar i
    record $ CEq a (TArr a' x)
    return x

runGather :: Context -> Term -> (Type, [Constr])
runGather c t = (a, acc s')
  where
    i = case IM.lookupMax (table c) of
        Nothing     -> 0
        Just (n, _) -> n + 1
    (a, s') = runState (gather c t) (GatherState i [])

-- Substitutions

type Sub = IM.IntMap Type

apply :: Sub -> Type -> Type
apply s (TVar n)   = IM.findWithDefault (TVar n) n s
apply s (TArr a b) = TArr (apply s a) (apply s b)

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

-- | Try to find a substitution that solves the given constraints. The only
-- way for this to fail is if it encounters a type constraint x = a for a
-- type variable x and type a with x free in a.
unify :: [Constr] -> Either String Sub
unify []        = Right IM.empty
unify ((CEq a b) : rest)
    | a == b    = unify rest
    | otherwise = case (a, b) of
        (TVar m, b)
            | m `IS.member` (free b) -> Left $
                "Recursive type constraint " <> show a <> " == " <> show b
            | otherwise              -> do
                let s = IM.singleton m b
                s' <- unify (map (applyBoth s) rest)
                return $ s' `after` s
        (a, TVar n)                  -> unify ((CEq b a) : rest)
        (TArr a a', TArr b b')       -> unify ((CEq a b) : (CEq a' b') : rest)
