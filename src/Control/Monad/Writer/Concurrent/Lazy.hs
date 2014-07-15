{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Maintainer  : me@joelt.io
-- Stability   : experimental
-- Portability : portable
--
-- A monad whose actions produce an output.
--
-- This module builds output lazily. For a strict version, see
-- "Control.Monad.Writer.Concurrent.Strict".
-----------------------------------------------------------------------------
module Control.Monad.Writer.Concurrent.Lazy (
    module Control.Monad.Writer,
    -- *** The WriterC monad transformer
    WriterC,

    -- *** Running WriterC actions
    runWriterC, execWriterC, mapWriterC,

    -- *** Running concurrent operations on a single input
    runWritersC, execWritersC,

    -- *** Lifting other operations
    liftCallCC, liftCatch
) where

import Control.Applicative
import Control.Concurrent.Lifted.Fork
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Writer

-- ---------------------------------------------------------------------------
-- | A concurrent monad transformer collecting output of type @w@.
--
-- This is very similar to @transformers@' 'WriterT', with the exception of
-- the 'MonadIO' constraint on every instance, which is necessary to
-- perform STM actions.
newtype WriterC w m a = WriterC
    { _runWriterC :: TVar w -> m (a, TVar w) }

instance MonadTrans (WriterC w) where
    lift m = WriterC $ \w -> do
        a <- m
        return (a, w)

instance MonadIO m => MonadIO (WriterC w m) where
    liftIO i = WriterC $ \w -> do
        a <- liftIO i
        return (a, w)

instance Functor m => Functor (WriterC w m) where
    fmap f m = WriterC $ \w ->
        fmap (\ ~(a, w') -> (f a, w')) $ _runWriterC m w

instance (Functor m, Monad m) => Applicative (WriterC w m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (WriterC w m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => Monad (WriterC w m) where
    return a = WriterC $ \w -> return (a, w)
    m >>= k = WriterC $ \w -> do
        ~(a, w') <- _runWriterC m w
        _runWriterC (k a) w'

instance MonadPlus m => MonadPlus (WriterC w m) where
    mzero = WriterC $ const mzero
    m `mplus` n = WriterC $ \w -> _runWriterC m w `mplus` _runWriterC n w

instance MonadFix m => MonadFix (WriterC w m) where
    mfix f = WriterC $ \w -> mfix $ \ ~(a, _) -> _runWriterC (f a) w

instance (Monoid w, MonadReader r m) => MonadReader r (WriterC w m) where
    ask = lift ask
    local f m = WriterC $ \w -> local f $ _runWriterC m w
    reader = lift . reader

instance (Monoid w, MonadIO m) => MonadWriter w (WriterC w m) where
    writer (a, w) = WriterC $ \tw -> do
        liftIO . atomically $ modifyTVar tw (<> w)
        return (a, tw)
    listen m = WriterC $ \tw -> do
        ~(a, tw') <- _runWriterC m tw
        w <- liftIO $ readTVarIO tw'
        return ((a, w), tw')
    pass m = WriterC $ \tw -> do
        ~((a, f), tw') <- _runWriterC m tw
        liftIO . atomically $ modifyTVar tw' f
        return (a, tw')

instance (MonadIO m, MonadCatch m) => MonadCatch (WriterC w m) where
    catch = liftCatch catch

instance (MonadIO m, MonadThrow m) => MonadThrow (WriterC w m) where
    throwM = liftIO . throwIO

instance (MonadIO m, MonadMask m) => MonadMask (WriterC w m) where
    mask a = WriterC $ \w -> mask $ \u -> _runWriterC (a $ q u) w where
        q u (WriterC f) = WriterC (u . f)
    uninterruptibleMask a =
        WriterC $ \w -> uninterruptibleMask $ \u -> _runWriterC (a $ q u) w where
        q u (WriterC f) = WriterC (u . f)

instance (Monoid w, MonadFork m) => MonadFork (WriterC w m) where
    fork = liftFork fork
    forkOn i = liftFork (forkOn i)
    forkOS = liftFork forkOS

liftFork :: Monad m => (m () -> m a) -> WriterC w m () -> WriterC w m a
liftFork f (WriterC m) = WriterC $ \w -> do
    tid <- f . voidM $ m w
    return (tid, w)
    where voidM = (>> return ())

-- | Unwrap a concurrent Writer monad computation as a function.
runWriterC :: MonadIO m
           => WriterC w m a -- ^ computation to execute
           -> TVar w -- ^ output channel
           -> m (a, w) -- ^ return value and collected output
runWriterC m tw = do
    (a, w) <- _runWriterC m tw
    w' <- liftIO $ readTVarIO w
    return (a, w')

-- | Unwrap a concurrent Writer monad computation as a function, discarding
-- the return value.
--
-- * @'execWriterC' m w = 'liftM' 'snd' ('runWriterC' m w)@
execWriterC :: MonadIO m
            => WriterC w m a -- ^ computation to execute
            -> TVar w -- ^ output channel
            -> m w -- ^ collected output
execWriterC m tw = liftM snd $ runWriterC m tw

-- | Map both the return value and output of a computation using the given
-- function.
mapWriterC :: (m (a, TVar w) -> n (b, TVar w)) -> WriterC w m a -> WriterC w n b
mapWriterC f m = WriterC $ \w -> f (_runWriterC m w)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: ((((a, TVar w) -> m (b, TVar w)) -> m (a, TVar w)) -> m (a, TVar w)) -> ((a -> WriterC w m b) -> WriterC w m a) -> WriterC w m a
liftCallCC callCC f = WriterC $ \w ->
    callCC $ \c ->
        _runWriterC (f (\a -> WriterC $ \_ -> c (a, w))) w

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a, TVar w) -> (e -> m (a, TVar w)) -> m (a, TVar w)) -> WriterC w m a -> (e -> WriterC w m a) -> WriterC w m a
liftCatch catchError m h =
    WriterC $ \w -> _runWriterC m w `catchError` \e -> _runWriterC (h e) w

-- | Run multiple Writer operations on the same value, returning the
-- resultant output and the value produced by each operation.
runWritersC :: (MonadFork m, Monoid w)
            => [WriterC w m a] -- ^ writer computations to execute
            -> m ([a], w) -- ^ return values and output
runWritersC ms = do
    output <- liftIO $ newTVarIO mempty
    mvs <- mapM (const (liftIO newEmptyMVar)) ms
    forM_ (zip mvs ms) $ \(mv, operation) -> fork $ do
        ~(res, _) <- runWriterC operation output
        liftIO $ putMVar mv res
    items <- forM mvs (liftIO . takeMVar)
    out <- liftIO $ readTVarIO output
    return (items, out)

-- | Run multiple Writer operations on the same value, returning the
-- resultant output.
execWritersC :: (MonadFork m, Monoid w)
             => [WriterC w m a] -- ^ writer computations to execute
             -> m w -- ^ output
execWritersC = liftM snd . runWritersC
