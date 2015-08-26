-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Struct.Order
  ( Order(..)
  , newOrder
  , insertAfter
  , delete
  , value
  , Key
  ) where

import Data.Struct.Internal.Label (Key)
import Data.Struct.Internal.Order
