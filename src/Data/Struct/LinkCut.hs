-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Struct.LinkCut
  ( LinkCut
  , new
  , link
  , cut
  , root
  , cost
  , parent
  , connected
  ) where

import Control.Monad.Primitive
import Data.Struct.Internal.LinkCut hiding (parent)

-- | O(log n). Find the parent of a node.
--
-- This will return 'Nil' if the parent does not exist.
--
-- Repeated calls on the same value without intermediate accesses are O(1).
parent :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> m (LinkCut a (PrimState m))
parent = up
