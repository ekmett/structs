{-# LANGUAGE Trustworthy #-}
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
  , insertAfter
  , delete
  , least
  , greatest
  , cutAfter
  , cutBefore
  , compareM
  ) where

import Data.Struct.Internal.Label
