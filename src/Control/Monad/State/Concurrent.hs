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
-----------------------------------------------------------------------------
module Control.Monad.State.Concurrent (
    module Control.Monad.State.Concurrent.Lazy
) where

import Control.Monad.State.Concurrent.Lazy
