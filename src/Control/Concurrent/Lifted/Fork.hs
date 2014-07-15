{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
    forkFinally,
    forkWithUnmask,
    forkOnWithUnmask
) where

import qualified Control.Concurrent as C
import           Control.Monad.Catch
import           Control.Monad.Trans.Identity
import           Control.Monad.Reader

-- | Generalize 'forkIO' to 'MonadIO'.
class MonadIO m => MonadFork m where
    fork :: m () -> m C.ThreadId
    forkOn :: Int -> m () -> m C.ThreadId
    forkOS :: m () -> m C.ThreadId

instance MonadFork IO where
    fork = C.forkIO
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 700
    forkOn _ = C.forkIO
#else
    forkOn = C.forkOn
#endif
    forkOS = C.forkOS

instance MonadFork m => MonadFork (IdentityT m) where
    fork (IdentityT m) = IdentityT (fork m)
    forkOn i (IdentityT m) = IdentityT (forkOn i m)
    forkOS (IdentityT m) = IdentityT (forkOS m)

instance MonadFork m => MonadFork (ReaderT r m) where
    fork (ReaderT m) = ReaderT (fork . m)
    forkOn i (ReaderT m) = ReaderT (forkOn i . m)
    forkOS (ReaderT m) = ReaderT (forkOS . m)

-- | Generalized 'C.forkFinally'.
forkFinally :: (MonadMask m, MonadCatch m, MonadFork m) => m a -> (Either SomeException a -> m ()) -> m C.ThreadId
forkFinally action andThen = mask $ \restore ->
    fork $ try (restore action) >>= andThen

-- | Generalized 'C.forkIOWithUnmask'.
forkWithUnmask :: (MonadMask m, MonadCatch m, MonadFork m) => ((forall a. m a -> m a) -> m ()) -> m C.ThreadId
forkWithUnmask = fork . mask

-- | Generalized 'C.forkOnWithUnmask'.
forkOnWithUnmask :: (MonadMask m, MonadCatch m, MonadFork m) => Int -> ((forall a. m a -> m a) -> m ()) -> m C.ThreadId
forkOnWithUnmask i = forkOn i . mask
