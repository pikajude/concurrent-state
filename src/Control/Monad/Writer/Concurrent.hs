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
-----------------------------------------------------------------------------
module Control.Monad.Writer.Concurrent (
    module Control.Monad.Writer.Concurrent.Lazy
) where

import Control.Monad.Writer.Concurrent.Lazy
