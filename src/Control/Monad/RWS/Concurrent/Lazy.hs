{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Maintainer  : me@joelt.io
-- Stability   : experimental
-- Portability : portable
--
-- Concurrent RWS monad, combining a Reader, a Writer, and a State monad.
--
-- This module performs computations lazily. For a strict version, see
-- "Control.Monad.RWS.Concurrent.Strict".
-----------------------------------------------------------------------------
module Control.Monad.RWS.Concurrent.Lazy (
    module Control.Monad.RWS,
    -- *** The RWSC monad transformer
    RWSC,

    -- *** Running RWSC actions
    runRWSC, evalRWSC, execRWSC, mapRWSC, withRWSC,

    -- *** Lifting other operations
    liftCallCCC, liftCatch
) where

import Control.Applicative
import Control.Concurrent.Lifted.Fork
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.RWS
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

-- ---------------------------------------------------------------------------
-- | A concurrent monad transformer reading an environment of type @r@,
-- collecting output of type @w@ and updating a state of type @s@ to an
-- inner monad @m@.
--
-- This is very similar to @transformers@' 'RWST', with the exception of
-- the 'MonadIO' constraint on every instance, which is necessary to
-- perform STM actions.
newtype RWSC r w s m a = RWSC
    { _runRWSC :: r -> TVar s -> TVar w -> m (a, TVar s, TVar w) }

instance MonadTrans (RWSC r w s) where
    lift m = RWSC $ \_ s w -> do
        a <- m
        return (a, s, w)

instance MonadIO m => MonadIO (RWSC r w s m) where
    liftIO i = RWSC $ \_ s w -> do
        a <- liftIO i
        return (a, s, w)

instance Functor m => Functor (RWSC r w s m) where
    fmap f m = RWSC $ \r s w ->
        fmap (\ ~(a, s', w') -> (f a, s', w')) $ _runRWSC m r s w

instance (Functor m, Monad m) => Applicative (RWSC r w s m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (RWSC r w s m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => Monad (RWSC r w s m) where
    return a = RWSC $ \_ s w -> return (a, s, w)
    m >>= k = RWSC $ \r s w -> do
        ~(a, s', w') <- _runRWSC m r s w
        _runRWSC (k a) r s' w'

instance MonadPlus m => MonadPlus (RWSC r w s m) where
    mzero = RWSC $ \_ _ _ -> mzero
    m `mplus` n = RWSC $ \r w s -> _runRWSC m r w s `mplus` _runRWSC n r w s

instance MonadFix m => MonadFix (RWSC r w s m) where
    mfix f = RWSC $ \r w s -> mfix $ \ ~(a, _, _) -> _runRWSC (f a) r w s

instance MonadReader r m => MonadReader r (RWSC r w s m) where
    ask = RWSC $ \r s w -> return (r, s, w)
    local f m = RWSC $ \r s w -> _runRWSC m (f r) s w
    reader f = RWSC $ \r s w -> return (f r, s, w)

instance (Monoid w, MonadIO m, MonadWriter w m) => MonadWriter w (RWSC r w s m) where
    writer (a, w) = RWSC $ \_ s tw -> do
        liftIO . atomically $ modifyTVar tw (<> w)
        return (a, s, tw)
    listen m = RWSC $ \r s tw -> do
        ~(a, s', tw') <- _runRWSC m r s tw
        w <- liftIO $ readTVarIO tw'
        return ((a, w), s', tw')
    pass m = RWSC $ \r s tw -> do
        ~((a, f), s', tw') <- _runRWSC m r s tw
        liftIO . atomically $ modifyTVar tw' f
        return (a, s', tw')

instance (MonadIO m, MonadState s m) => MonadState s (RWSC r w s m) where
    get = RWSC $ \_ tv w -> do
        s <- liftIO $ readTVarIO tv
        return (s, tv, w)
    state f = RWSC $ \_ tv w -> do
        newval <- liftIO . atomically $ do
            old <- readTVar tv
            let ~(a, s) = f old
            _ <- swapTVar tv s
            return a
        return (newval, tv, w)

instance (MonadIO m, MonadCatch m) => MonadCatch (RWSC r w s m) where
    throwM = liftIO . throwIO
    catch = liftCatch catch
    mask a = RWSC $ \r s w -> mask $ \u -> _runRWSC (a $ q u) r s w where
        q u (RWSC f) = RWSC (((u .) .) . f)
    uninterruptibleMask a =
        RWSC $ \r s w -> uninterruptibleMask $ \u -> _runRWSC (a $ q u) r s w where
        q u (RWSC f) = RWSC (((u .) .) . f)

instance (Monoid w, MonadIO m, MonadReader r m, MonadWriter w m, MonadState s m) => MonadRWS r w s (RWSC r w s m)

instance (Monoid w, MonadFork m) => MonadFork (RWSC r w s m) where
    fork = liftFork fork
    forkOn i = liftFork (forkOn i)
    forkOS = liftFork forkOS

liftFork :: Monad m => (m () -> m a) -> RWSC r w s m () -> RWSC r w s m a
liftFork f (RWSC m) = RWSC $ \r w s -> do
    tid <- f . voidM $ m r w s
    return (tid, w, s)
    where voidM = (>> return ())


-- | Unwrap a concurrent RWS monad computation as a function.
runRWSC :: MonadIO m
        => RWSC r w s m a -- ^ computation to execute
        -> r -- ^ environment to read
        -> TVar s -- ^ state to modify
        -> TVar w -- ^ output channel
        -> m (a, s, w) -- ^ return value, final state, and collected output
runRWSC m r ts tw = do
    (a, s, w) <- _runRWSC m r ts tw
    s' <- liftIO $ readTVarIO s
    w' <- liftIO $ readTVarIO w
    return (a, s', w')

-- | Unwrap a concurrent RWS monad computation as a function, discarding
-- the final state.
evalRWSC :: MonadIO m
        => RWSC r w s m a -- ^ computation to execute
        -> r -- ^ environment to read
        -> TVar s -- ^ state to modify
        -> TVar w -- ^ output channel
         -> m (a, w) -- ^ return value and collected output
evalRWSC m r ts tw = liftM (\(a,_,w) -> (a,w)) $ runRWSC m r ts tw

-- | Unwrap a concurrent RWS monad computation as a function, discarding
-- the return value.
execRWSC :: MonadIO m
        => RWSC r w s m a -- ^ computation to execute
        -> r -- ^ environment to read
        -> TVar s -- ^ state to modify
        -> TVar w -- ^ output channel
         -> m (s, w) -- ^ final state and collected output
execRWSC m r ts tw = liftM (\(_,s,w) -> (s,w)) $ runRWSC m r ts tw

-- | Map the inner computation using the given function.
--
-- * @'runRWSC' ('mapRWSC' f m) r w s = f ('runRWSC' m r w s)@
mapRWSC :: (m (a, TVar s, TVar w) -> n (b, TVar s, TVar w)) -> RWSC r w s m a -> RWSC r w s n b
mapRWSC f m = RWSC $ \r w s -> f (_runRWSC m r w s)

-- | @'withRWSC' f m@ executes action @m@ with an initial environment and
-- state modified by applying @f@.
--
-- * @'runRWSC' ('withRWSC' f m) r s w = uncurry3 ('runRWSC' m) (f r s w)@
withRWSC :: (r' -> TVar s -> TVar w -> (r, TVar s, TVar w)) -> RWSC r w s m a -> RWSC r' w s m a
withRWSC f m = RWSC $ \r s w -> uncurry3 (_runRWSC m) (f r s w) where
    uncurry3 q (a,b,c) = q a b c

-- | Uniform lifting of a @callCC@ operation to the new monad.
liftCallCCC :: ((((a, TVar s, TVar w) -> m (b, TVar s, TVar w)) -> m (a, TVar s, TVar w)) -> m (a, TVar s, TVar w)) -> ((a -> RWSC r w s m b) -> RWSC r w s m a) -> RWSC r w s m a
liftCallCCC callCC f = RWSC $ \r s w ->
    callCC $ \c ->
        _runRWSC (f (\a -> RWSC $ \_ _ _ -> c (a, s, w))) r s w

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (m (a, TVar s, TVar w) -> (e -> m (a, TVar s, TVar w)) -> m (a, TVar s, TVar w)) -> RWSC l w s m a -> (e -> RWSC l w s m a) -> RWSC l w s m a
liftCatch catchError m h =
    RWSC $ \r s w -> _runRWSC m r s w `catchError` \e -> _runRWSC (h e) r s w
