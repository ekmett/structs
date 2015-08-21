{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
