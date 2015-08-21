{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Struct.Order
  ( Order(..)
  , newOrder
  ) where

import Control.Monad.Primitive
import Data.Struct.Internal
import Data.Struct.Label.Internal

--------------------------------------------------------------------------------
-- Order Maintenance
--------------------------------------------------------------------------------

newtype Order s = Order { runOrder :: Object s }

parent :: Slot Order Label
parent = slot 3
{-# INLINE parent #-}

instance Eq (Order s) where (==) = eqStruct

instance Struct Order where
  struct _ = Dict

instance Intrusive Order

makeOrder :: PrimMonad m => Label (PrimState m) -> Key -> Order (PrimState m) -> Order (PrimState m) -> m (Order (PrimState m))
makeOrder p a l r = st $ do
  this <- alloc 4
  set parent this p
  setField key this a
  set next this l
  set prev this r
  return this
{-# INLINE makeOrder #-}

-- -- | O(1) insertAfterOrder
-- insertAfterOrder :: PrimMonad m => Order (PrimState m) -> m (Order (PrimState m))

-- | O(1) compareM
instance OrdM Order where
  compareM i j = st $ do
    ui <- get parent i
    uj <- get parent j
    compareM ui uj >>= \case
      EQ -> compare <$> getField key i <*> getField key j
      x -> return x
  {-# INLINE compareM #-}

newOrder :: PrimMonad m => m (Order (PrimState m))
newOrder = st $ do
  l <- newLabel
  makeOrder l midBound Nil Nil
{-# INLINE newOrder #-}
