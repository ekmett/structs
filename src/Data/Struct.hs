{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
#ifndef HLINT
  , pattern Nil
#endif
  , isNil
  , Slot
  , Precomposable(..)
  , slot
  , get
  , set
  , Field
  , field
  , unboxedField
  , getField
  , setField
  ) where

import Data.Struct.Internal
