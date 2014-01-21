{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Writer.Concurrent (
    module Control.Monad.Writer,
    runWriterC, execWriterC
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer

newtype MonadIO m => WriterC w m a = WriterC { _runWriterC :: TVar w -> m (a, TVar w) }

instance (Monoid w, MonadIO m) => Monad (WriterC w m) where
    return a = WriterC $ \t -> return (a, t)
    m >>= k = WriterC $ \t -> do
        ~(a, w) <- _runWriterC m t
        ~(b, _) <- _runWriterC (k a) t
        return (b, w)

instance (Monoid w, MonadIO m) => MonadWriter w (WriterC w m) where
    tell w = WriterC $ \tv -> do
        liftIO . atomically $ modifyTVar tv (<> w)
        return ((), tv)
    listen m = WriterC $ \tv -> do
        (a, t') <- _runWriterC m tv
        q <- liftIO $ readTVarIO t'
        return ((a, q), t')
    pass m = WriterC $ \tv -> do
        ((a, f), w) <- _runWriterC m tv
        liftIO . atomically $ modifyTVar tv f
        return (a, w)

instance (Monoid w, MonadIO m) => MonadIO (WriterC w m) where
    liftIO a = WriterC $ \tv -> do
        s <- liftIO a
        return (s, tv)

runWriterC :: MonadIO m => TVar t1 -> WriterC t1 m t -> m (t, t1)
runWriterC tv m = do
    (a, tv') <- _runWriterC m tv
    w <- liftIO $ readTVarIO tv'
    return (a, w)

execWriterC :: MonadIO m => TVar t1 -> WriterC t1 m t -> m t1
execWriterC tv m = liftM snd (runWriterC tv m)
