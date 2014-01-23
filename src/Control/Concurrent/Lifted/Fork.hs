{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Maintainer  : me@joelt.io
-- Stability   : experimental
-- Portability : portable
--
-- Generalizes `forkIO' to MonadIO.
-----------------------------------------------------------------------------
module Control.Concurrent.Lifted.Fork (
    MonadFork(..),
    forkFinally
) where

import qualified Control.Concurrent as C
import           Control.Monad.Catch
import           Control.Monad.Reader

-- | Generalize 'forkIO' to 'MonadIO'.
class (MonadIO m, MonadCatch m) => MonadFork m where
    fork :: m () -> m C.ThreadId
    forkOn :: Int -> m () -> m C.ThreadId
    forkOS :: m () -> m C.ThreadId

instance MonadFork IO where
    fork = C.forkIO
    forkOn = C.forkOn
    forkOS = C.forkOS

instance MonadFork m => MonadFork (ReaderT r m) where
    fork (ReaderT m) = ReaderT (fork . m)
    forkOn i (ReaderT m) = ReaderT (forkOn i . m)
    forkOS (ReaderT m) = ReaderT (forkOS . m)

-- | Generalized 'C.forkFinally'.
forkFinally :: MonadFork m => m a -> (Either SomeException a -> m ()) -> m C.ThreadId
forkFinally action andThen = mask $ \restore ->
    fork $ try (restore action) >>= andThen
