{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Struct
  ( Struct(..)
  , Object
  , destruct
  , construct
  , eqStruct
  , alloc
  -- * Nil
#ifndef HLINT
  , pattern Nil
#endif
  , isNil
  , NullPointerException(..)
  -- * Slots and Fields
  , Slot, slot
  , get, set
  , Field, field
  , unboxedField
  , getField, setField, modifyField, modifyField'
  , Precomposable(..)
  ) where

import Data.Struct.Internal
