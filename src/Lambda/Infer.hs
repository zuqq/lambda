module Lambda.Infer where

import Control.Monad.Trans.State (State)
import Lens.Micro                (Lens')
import Lens.Micro.Mtl            ((+=), modifying, use, view)
import qualified Control.Monad.Trans.State as State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import Lambda.Type
import Lambda.Typed
import qualified Lambda.Type    as Type    (Type (..))
import qualified Lambda.Typed   as Typed   (Term (..))
import qualified Lambda.Untyped as Untyped (Term (..))

-- | A constraint is an equality between two types.
type Constraint = (Type, Type)

-- | Apply a substitution to both sides of a constraint.
transform :: Sub -> Constraint -> Constraint
transform s (a, b) = (apply s a, apply s b)

data GatherState = GatherState
    Int
    -- ^ Index of the next fresh type variable.
    [Constraint]
    -- ^ List of collected constraints.
    deriving (Eq, Read, Show)

index :: Lens' GatherState Int
index p (GatherState i cs) = fmap (`GatherState` cs) (p i)

collected :: Lens' GatherState [Constraint]
collected q (GatherState i cs) = fmap (i `GatherState`) (q cs)

type Gather a = State GatherState a

-- | Get a fresh type variable.
fresh :: Gather Type
fresh = do
    i <- use index
    index += 1
    pure (Type.Var i)

-- | Record a constraint.
record :: Constraint -> Gather ()
record c = modifying collected (c :)

-- | Gather the type constraints for the given term.
--
-- Variables that are not typed by the context get fresh type variables,
-- keeping their types as general as possible.
gather :: Context -> Untyped.Term -> Gather Typed.Term
gather c (Untyped.Var n)    = case Map.lookup n c of
    Nothing -> fmap (Typed.Var n) fresh
    Just a  -> pure (Typed.Var n a)
gather c (Untyped.Abs t)    = do
    x <- fresh
    b <- gather (bind x c) t
    pure (Typed.Abs b (Type.Arr x (typeof b)))
gather c (Untyped.App t t') = do
    a  <- gather c t
    a' <- gather c t'
    x  <- fresh
    record (typeof a, Type.Arr (typeof a') x)
    pure (Typed.App a a' x)

runGather :: Context -> Untyped.Term -> (Typed.Term, [Constraint])
runGather c t = (a, view collected s')
  where
    bound   = foldr (Set.union . free) Set.empty c
    next    = if Set.null bound then 0 else Set.findMax bound + 1
    (a, s') = State.runState (gather c t) (GatherState next [])

-- | Try to find a substitution that solves the given constraints.
unify :: [Constraint] -> Maybe Sub
unify []              = Just Map.empty
unify ((a, b) : rest) = if a == b
    then unify rest
    else case (a, b) of
        (Type.Var m, _)
            | m `Set.member` free b    -> Nothing
            | otherwise                ->
                let s = Map.singleton m b
                in fmap (`after` s) (unify (fmap (transform s) rest))
        (_, Type.Var _)                -> unify ((b, a) : rest)
        (Type.Arr c c', Type.Arr d d') -> unify ((c, d) : (c', d') : rest)

-- | Try to infer the type of the given term.
--
-- The return value is a typed term, with the substitution returned by 'unify'
-- already applied.
infer :: Context -> Untyped.Term -> Maybe Typed.Term
infer g t = do
    s <- unify cs
    pure (sub s n)
  where
    (n, cs) = runGather g t
