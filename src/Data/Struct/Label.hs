-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Struct.Label
  ( Label
  , newLabel
  , IsLabel
    ( insertAfter
    , least, greatest
    , cutAfter, cutBefore
    , compareM
    , next
    , prev
    , key
    )
  , keys
  , Key
  ) where

import Data.Struct.Internal.Label
