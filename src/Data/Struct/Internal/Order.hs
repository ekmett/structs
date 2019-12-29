{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Struct.Internal.Order where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Struct.Internal
import Data.Struct.Internal.Label (Label, Key)
import qualified Data.Struct.Label as Label
import qualified Data.Struct.Internal.Label as Label (key)
import Data.Word

--------------------------------------------------------------------------------
-- * Order Maintenance
--------------------------------------------------------------------------------

-- | This structure maintains an order-maintenance structure as two levels of list-labeling.
--
-- The upper labeling scheme holds @(n / log w)@ elements in a universe of size @w^2@, operating in O(log n) amortized time per operation.
--
-- It is accelerated by an indirection structure where each smaller universe holds O(log w) elements, with total label space @2^log w = w@ and O(1) expected update cost, so we
-- can charge rebuilds to the upper structure to the lower structure.
--
-- Every insert to the upper structure is amortized across @O(log w)@ operations below.
--
-- This means that inserts are O(1) amortized, while comparisons remain O(1) worst-case.

newtype Order a s = Order { runOrder :: Object s }

instance Eq (Order a s) where (==) = eqStruct

instance Struct (Order a)

key :: Field (Order a) Key
key = field 0
{-# INLINE key #-}

value :: Field (Order a) a
value = field 1
{-# INLINE value #-}

next :: Slot (Order a) (Order a)
next = slot 2
{-# INLINE next #-}

prev :: Slot (Order a) (Order a)
prev = slot 3
{-# INLINE prev #-}

parent :: Slot (Order a) Label
parent = slot 4
{-# INLINE parent #-}

makeOrder :: PrimMonad m => Label (PrimState m) -> Key -> a -> Order a (PrimState m) -> Order a (PrimState m) -> m (Order a (PrimState m))
makeOrder mom a v p n = st $ do
  this <- alloc 5
  set parent this mom
  setField key this a
  setField value this v
  set prev this p
  set next this n
  return this
{-# INLINE makeOrder #-}

-- | O(1) compareM, O(1) amortized insert
compareM :: PrimMonad m => Order a (PrimState m) -> Order a (PrimState m) -> m Ordering
compareM i j
  | isNil i || isNil j = throw NullPointerException
  | otherwise = st $ do
  ui <- get parent i
  uj <- get parent j
  xs <- Label.compareM ui uj
  case xs of
    EQ -> compare <$> getField key i <*> getField key j
    x -> return x
{-# INLINE compareM #-}

insertAfter :: PrimMonad m => Order a (PrimState m) -> a -> m (Order a (PrimState m))
insertAfter n0 a1 = st $ do
  when (isNil n0) $ throw NullPointerException
  mom <- get parent n0
  k0 <- getField key n0
  n2 <- get next n0
  k2 <- if isNil n2 then return maxBound else getField key n2
  let !k1 = k0 + unsafeShiftR (k2 - k0) 1
  n1 <- makeOrder mom k1 a1 n0 n2
  unless (isNil n2) $ set prev n2 n1
  set next n0 n1
  when (k0 + 1 == k2) $ rewind mom n0 -- we have a collision, rebalance
  return n1
 where
  -- find the smallest sibling
  rewind :: Label s -> Order a s -> ST s ()
  rewind mom this = do
    p <- get prev this
    if isNil p then rebalance mom mom this 0 64
    else do
      dad <- get parent p
      if mom == dad then rewind mom p
      else rebalance mom mom p 0 64

  -- break up the family
  rebalance :: Label s -> Label s -> Order a s -> Word64 -> Int -> ST s ()
  rebalance mom dad this k j = unless (isNil this) $ do
    guardian <- get parent this
    when (mom == guardian) $ do
      setField key this k
      set parent this dad
      n <- get next this
      if j > 0 then rebalance mom dad n (k + deltaU) (j-1)
      else do
        stepdad <- Label.insertAfter dad
        rebalance mom stepdad n deltaU logU

delete :: PrimMonad m => Order a (PrimState m) -> m ()
delete this = st $ do
  when (isNil this) $ throw NullPointerException
  mom <- get parent this

  p <- get prev this
  n <- get next this

  set prev this Nil
  set next this Nil

  x <- if isNil p then return False
  else do
    set next p n
    pmom <- get parent p
    return (mom == pmom)

  y <- if isNil n then return False
  else do
    set prev n p
    nmom <- get parent n
    return (mom == nmom)

  unless (x || y) $ Label.delete mom
{-# INLINE delete #-}

logU :: Int
logU = 64

loglogU :: Int
loglogU = 6

deltaU :: Key
deltaU = unsafeShiftR maxBound loglogU -- U / log U

new :: PrimMonad m => a -> m (Order a (PrimState m))
new a = st $ do
  l <- Label.new
  makeOrder l (unsafeShiftR maxBound 1) a Nil Nil
{-# INLINE new #-}

keys :: PrimMonad m => Order a (PrimState m) -> m (Key, Key)
keys this = st $ do
  mom <- get parent this
  (,) <$> getField Label.key mom <*> getField key this
{-# INLINE keys #-}
