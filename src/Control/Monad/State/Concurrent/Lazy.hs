{-# LANGUAGE CPP #-}
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

    -- *** Running concurrent operations on a single input
    runStatesC, evalStatesC, execStatesC,

    -- *** Lifting other operations
    liftCallCC, liftCatch, liftListen, liftPass
) where

import Control.Applicative
import Control.Concurrent.Lifted.Fork
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

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
    get = StateC $ \tv -> do
        s <- liftIO $ readTVarIO tv
        return (s, tv)
    state f = StateC $ \tv -> do
        newval <- liftIO . atomically $ do
            old <- readTVar tv
            let ~(a, s) = f old
            _ <- swapTVar tv s
            return a
        return (newval, tv)

instance (MonadIO m, MonadFix m) => MonadFix (StateC s m) where
    mfix f = StateC $ \s -> mfix $ \ ~(a, _) -> _runStateC (f a) s

instance MonadIO m => MonadIO (StateC s m) where
    liftIO i = StateC $ \s -> do
        a <- liftIO i
        return (a, s)

instance (MonadIO m, MonadCatch m) => MonadCatch (StateC s m) where
    catch = liftCatch catch

instance (MonadIO m, MonadThrow m) => MonadThrow (StateC s m) where
    throwM = liftIO . throwIO

instance (MonadIO m, MonadCatch m, MonadMask m) => MonadMask (StateC s m) where
    mask a = StateC $ \tv -> mask $ \u -> _runStateC (a $ q u) tv where
        q u (StateC f) = StateC (u . f)
    uninterruptibleMask a =
        StateC $ \tv -> uninterruptibleMask $ \u -> _runStateC (a $ q u) tv where
        q u (StateC f) = StateC (u . f)

instance MonadFork m => MonadFork (StateC s m) where
    fork = liftFork fork
    forkOn i = liftFork (forkOn i)
    forkOS = liftFork forkOS

liftFork :: Monad m => (m () -> m a) -> StateC t m () -> StateC t m a
liftFork f (StateC m) = StateC $ \tv -> do
    tid <- f . voidM $ m tv
    return (tid, tv)
    where voidM = (>> return ())

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

-- | Uniform lifting of a @callCC@ operation to the new monad.
liftCallCC :: ((((a, TVar s) -> m (b, TVar s)) -> m (a, TVar s)) -> m (a, TVar s)) ->
    ((a -> StateC s m b) -> StateC s m a) -> StateC s m a
liftCallCC callCC f = StateC $ \tv ->
    callCC $ \c ->
        _runStateC (f (\a -> StateC $ \_ -> c (a, tv))) tv

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a, TVar s) -> (e -> m (a, TVar s)) -> m (a, TVar s)) ->
    StateC s m a -> (e -> StateC s m a) -> StateC s m a
liftCatch catchError m h =
    StateC $ \s -> _runStateC m s `catchError` \e -> _runStateC (h e) s

-- | Lift a @listen@ operation to the new monad.
liftListen :: Monad m =>
    (m (a, TVar s) -> m ((a, TVar s), w)) -> StateC s m a -> StateC s m (a,w)
liftListen listen m = StateC $ \tv -> do
    ~((a, s'), w) <- listen (_runStateC m tv)
    return ((a, w), s')

-- | Lift a @pass@ operation to the new monad.
liftPass :: Monad m =>
    (m ((a, TVar s), b) -> m (a, TVar s)) -> StateC s m (a, b) -> StateC s m a
liftPass pass m = StateC $ \tv -> pass $ do
    ~((a, f), s') <- _runStateC m tv
    return ((a, s'), f)

-- | Run multiple state operations on the same value, returning the
-- resultant state and the value produced by each operation.
runStatesC :: MonadFork m
           => [StateC s m a] -- ^ state-passing computations to execute
           -> s -- ^ initial state
           -> m ([a], s) -- ^ return values and final state
runStatesC ms s = do
    v <- liftIO $ newTVarIO s
    mvs <- mapM (const (liftIO newEmptyMVar)) ms
    forM_ (zip mvs ms) $ \(mv, operation) -> fork $ do
        res <- evalStateC operation v
        liftIO $ putMVar mv res
    items <- forM mvs (liftIO . takeMVar)
    end <- liftIO $ readTVarIO v
    return (items, end)

-- | Run multiple state operations on the same value, returning all values
-- produced by each operation.
--
-- * @'evalStatesC' ms s = 'liftM' 'fst' ('runStatesC' ms s)@
evalStatesC :: MonadFork m
            => [StateC s m a] -- ^ state-passing computations to execute
            -> s -- ^ initial state
            -> m [a] -- ^ return values
evalStatesC ms s = liftM fst $ runStatesC ms s

-- | Run multiple state operations on the same value, returning the
-- resultant state.
--
-- * @'execStatesC' ms s = 'liftM' 'snd' ('runStatesC' ms s)@
execStatesC :: MonadFork m
            => [StateC s m a] -- ^ state-passing computations to execute
            -> s -- ^ initial state
            -> m s -- ^ final state
execStatesC ms s = liftM snd $ runStatesC ms s
