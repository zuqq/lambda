module Lambda.Infer where

import Control.Monad.Trans.State (State)
import Data.Map                  (Map)
import Data.Set                  (Set)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Control.Monad.Trans.State as State

import Lambda.Type
import Lambda.Typed
import qualified Lambda.Type    as Type    (Type (..))
import qualified Lambda.Typed   as Typed   (Term (..))
import qualified Lambda.Untyped as Untyped (Term (..))

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
    i <- State.gets ix
    State.modify $ \s -> s {ix = i + 1}
    pure $ Type.Var i

-- | Record a constraint.
record :: Constr -> Gather ()
record c = State.modify $ \s -> s {acc = c : acc s}

-- | Gather the type constraints for the given term. Variables that are not
-- typed by the context get fresh type variables, keeping their types as
-- general as possible.
gather :: Context -> Untyped.Term -> Gather Typed.Term
gather g (Untyped.Var n) = case Map.lookup n g of
    Nothing -> Typed.Var n <$> fresh
    Just a  -> pure $ Typed.Var n a
gather g (Untyped.Abs t) = do
    x <- fresh
    b <- gather (bind x g) t
    pure $ Typed.Abs b (Type.Arr x (typeof b))
gather g (Untyped.App t t') = do
    a  <- gather g t
    a' <- gather g t'
    x  <- fresh
    record (typeof a, Type.Arr (typeof a') x)
    pure $ Typed.App a a' x

runGather :: Context -> Untyped.Term -> (Typed.Term, [Constr])
runGather g t = (a, acc s')
  where
    allTVar = foldr (Set.union . free) Set.empty g
    newTVar = if Set.null allTVar then 0 else Set.findMax allTVar + 1
    (a, s') = State.runState (gather g t) (GatherState newTVar [])

-- | Try to find a substitution that solves the given constraints.
unify :: [Constr] -> Maybe Sub
unify []              = Just Map.empty
unify ((a, b) : rest) = if a == b
    then unify rest
    else case (a, b) of
        (Type.Var m, _)
            | m `Set.member` free b    -> Nothing
            | otherwise                ->
                let s = Map.singleton m b
                in (`after` s) <$> unify (map (transform s) rest)
        (_, Type.Var _)                -> unify $ (b, a) : rest
        (Type.Arr c c', Type.Arr d d') -> unify $ (c, d) : (c', d') : rest

-- | Try to infer the type of the given term.
--
-- The return value is the typed AST, with the subsitution returned by 'unify'
-- already applied.
infer :: Context -> Untyped.Term -> Maybe Typed.Term
infer g t = do
    s <- unify cs
    pure $ sub s n
  where
    (n, cs) = runGather g t
