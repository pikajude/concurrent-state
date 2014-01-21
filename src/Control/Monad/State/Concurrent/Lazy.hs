{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Maintainer  : me@joelt.io
-- Stability   : experimental
-- Portability : portable
--
-- Concurrent state monad, providing a State-like interface but allowing
-- for multiple threads to operate on the same value simultaneously.
--
-- This module performs state computations lazily. For a strict version,
-- see "Control.Monad.State.Concurrent.Strict".
-----------------------------------------------------------------------------
module Control.Monad.State.Concurrent.Lazy (
    module Control.Monad.State,
    -- *** The StateC monad transformer
    StateC,

    -- *** Concurrent state operations
    runStateC, evalStateC, execStateC,

    -- *** Lifting other operations
    liftCallCCC, liftCallCCC', liftCatchC, liftListenC, liftPassC
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State

-- ---------------------------------------------------------------------------
-- | A concurrent state transformer monad parameterized by:
--
--  * @s@ - The state. This is contained in a 'TVar'.
--
--  * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ performs
-- actions atomically on the held 'TVar'.
--
-- This is very similar to @transformers@' 'StateT', with the exception of
-- the 'MonadIO' constraint on every instance, which is necessary to
-- perform STM actions.
newtype StateC s m a = StateC { _runStateC :: TVar s -> m (a, TVar s) }

instance MonadTrans (StateC s) where
    lift m = StateC $ \s -> do
        a <- m
        return (a, s)

instance (Functor m, MonadIO m) => Functor (StateC s m) where
    fmap f m = StateC $ \s ->
        fmap (\ ~(a, s') -> (f a, s')) $ _runStateC m s

instance (Functor m, MonadIO m) => Applicative (StateC s m) where
    pure = return
    (<*>) = ap

instance (MonadIO m, Functor m, MonadPlus m) => Alternative (StateC s m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus m, MonadIO m) => MonadPlus (StateC s m) where
    mzero = StateC $ const mzero
    m `mplus` n = StateC $ \s -> _runStateC m s `mplus` _runStateC n s

instance MonadIO m => Monad (StateC s m) where
    return a = StateC $ \s -> return (a, s)
    m >>= k = StateC $ \s -> do
        ~(a, s') <- _runStateC m s
        _runStateC (k a) s'

instance (Functor m, MonadIO m) => MonadState s (StateC s m) where
    get = StateC $ \s -> do
        m <- liftIO (readTVarIO s)
        return (m, s)
    put s = StateC $ \tv -> do
        liftIO . atomically $ swapTVar tv s
        return ((), tv)

instance (MonadIO m, MonadFix m) => MonadFix (StateC s m) where
    mfix f = StateC $ \s -> mfix $ \ ~(a, _) -> _runStateC (f a) s

instance MonadIO m => MonadIO (StateC s m) where
    liftIO i = StateC $ \s -> do
        a <- liftIO i
        return (a, s)

-- | Unwrap a concurrent state monad computation as a function.
runStateC :: MonadIO m
          => StateC s m a -- ^ state-passing computation to execute
          -> TVar s -- ^ initial state
          -> m (a, s) -- ^ return value and final state
runStateC m s = do
    ~(a, b) <- _runStateC m s
    r <- liftIO $ readTVarIO b
    return (a, r)

-- | Evaluate a concurrent state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateC' m s = 'liftM' 'fst' ('runStateC' m s)@
evalStateC :: MonadIO m
           => StateC s m a -- ^ state-passing computation to execute
           -> TVar s -- ^ initial state
           -> m a -- ^ return value
evalStateC m s = liftM fst $ runStateC m s

-- | Execute a concurrent state computation with the given initial state and return
-- the final state, discarding the final value.
--
-- * @'execStateC' m s = 'liftM' 'snd' ('runStateC' m s)@
execStateC :: MonadIO m
           => StateC s m a -- ^ state-passing computation to execute
           -> TVar s -- ^ initial state
           -> m s -- ^ final state
execStateC m s = liftM snd $ runStateC m s

-- | Uniform lifting of a @callCC@ operation to the new monad. This version
-- rolls back to the original 'TVar' upon entering the continuation.
liftCallCCC :: ((((a, TVar s) -> m (b, TVar s)) -> m (a, TVar s)) -> m (a, TVar s)) ->
    ((a -> StateC s m b) -> StateC s m a) -> StateC s m a
liftCallCCC callCC f = StateC $ \tv ->
    callCC $ \c ->
        _runStateC (f (\a -> StateC $ \_ -> c (a, tv))) tv

-- | In-situ lifting of a @callCC@ operation to the new monad. This version
-- uses the current 'TVar' upon entering the continuation. It does not
-- satisfy the laws of a monad transformer.
liftCallCCC' :: ((((a, TVar s) -> m (b, TVar s)) -> m (a, TVar s))-> m (a, TVar s)) ->
    ((a -> StateC s m b) -> StateC s m a) -> StateC s m a
liftCallCCC' callCC f = StateC $ \tv ->
    callCC $ \c ->
        _runStateC (f (\a -> StateC $ \s' -> c (a, s'))) tv

-- | Lift a @catchError@ operation to the new monad.
liftCatchC :: (m (a, TVar s) -> (e -> m (a, TVar s)) -> m (a, TVar s)) ->
    StateC s m a -> (e -> StateC s m a) -> StateC s m a
liftCatchC catchError m h =
    StateC $ \s -> _runStateC m s `catchError` \e -> _runStateC (h e) s

-- | Lift a @listen@ operation to the new monad.
liftListenC :: Monad m =>
    (m (a, TVar s) -> m ((a, TVar s), w)) -> StateC s m a -> StateC s m (a,w)
liftListenC listen m = StateC $ \tv -> do
    ~((a, s'), w) <- listen (_runStateC m tv)
    return ((a, w), s')

-- | Lift a @pass@ operation to the new monad.
liftPassC :: Monad m =>
    (m ((a, TVar s), b) -> m (a, TVar s)) -> StateC s m (a, b) -> StateC s m a
liftPassC pass m = StateC $ \tv -> pass $ do
    ~((a, f), s') <- _runStateC m tv
    return ((a, s'), f)
