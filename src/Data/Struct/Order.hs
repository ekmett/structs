{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GHCForeignImportPrim #-}
module Data.Struct.Order
  ( Order(..)
  , newOrder
  , insertAfterOrder
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Struct.Internal
import Data.Struct.Label.Internal
import Data.Word

--------------------------------------------------------------------------------
-- * Order Maintenance
--------------------------------------------------------------------------------

-- | This structure maintains an order-maintenance structure as two levels of list-labeling.
--
-- The upper labeling scheme holds (n / log w) elements in a universe of size w^2, operating in O(log n) amortized time per operation.
--
-- It is accelerated by an indirection structure where each smaller universe has size O(log n), but O(1) expected update cost, so we
-- can charge rebuilds to the upper structure to the lower structure.
--
-- This means that inserts are O(1) amortized, while comparisons remain O(1) worst-case.

newtype Order s = Order { runOrder :: Object s }

parent :: Slot Order Label
parent = slot 3
{-# INLINE parent #-}

instance Eq (Order s) where (==) = eqStruct

instance Struct Order where
  struct _ = Dict

instance IsLabel Order

makeOrder :: PrimMonad m => Label (PrimState m) -> Key -> Order (PrimState m) -> Order (PrimState m) -> m (Order (PrimState m))
makeOrder mom a p n = st $ do
  this <- alloc 4
  set parent this mom
  setField key this a
  set prev this p
  set next this n
  return this
{-# INLINE makeOrder #-}

logU :: Int
logU = 64

loglogU :: Int
loglogU = 6

deltaU :: Key
deltaU = unsafeShiftR maxBound loglogU -- U / log U

-- | O(1) compareM
instance OrdM Order where
  compareM i j = st $ do
    ui <- get parent i
    uj <- get parent j
    compareM ui uj >>= \case
      EQ -> compare <$> getField key i <*> getField key j
      x -> return x
  {-# INLINE compareM #-}

-- | O(1) amortized insert
insertAfterOrder :: PrimMonad m => Order (PrimState m) -> m (Order (PrimState m))
insertAfterOrder n0 = st $ do
  mom <- get parent n0
  k0 <- getField key n0
  n2 <- get next n0
  k2 <- if isNil n2 then return maxBound else getField key n2
  let !k1 = k0 + unsafeShiftR (k2 - k0) 1
  n1 <- makeOrder mom k1 n0 n2
  unless (isNil n2) $ set prev n2 n1
  set next n0 n1
  when (k0 + 1 == k2) $ rewind mom n0 -- we have a collision, rebalance
  return n1
 where
  -- find the smallest sibling
  rewind :: Label s -> Order s -> ST s ()
  rewind mom this = get prev this >>= \p -> if
    | isNil p    -> rebalance mom mom this 0 64
    | otherwise  -> get parent p >>= \dad -> if
      | mom == dad -> rewind mom p
      | otherwise  -> rebalance mom mom p 0 64

  -- break up the family
  rebalance :: Label s -> Label s -> Order s -> Word64 -> Int -> ST s ()
  rebalance mom dad this k j = unless (isNil this) $ do
    guardian <- get parent this
    when (mom == guardian) $ do
      setField key this k
      set parent this dad
      n <- get next this
      if | j > 0 -> rebalance mom dad n (k + deltaU) (j-1)
         | otherwise -> do
             stepdad <- insertAfterLabel dad
             rebalance mom stepdad n deltaU logU

newOrder :: PrimMonad m => m (Order (PrimState m))
newOrder = st $ do
  l <- newLabel
  makeOrder l (unsafeShiftR maxBound 1) Nil Nil
{-# INLINE newOrder #-}

value :: PrimMonad m => Order (PrimState m) -> m (Key, Key)
value this = st $ do
  mom <- get parent this
  (,) <$> getField key mom <*> getField key this
{-# INLINE value #-}

-- O(n) sweep to the right from the current node, showing all values for debugging
values :: PrimMonad m => Order (PrimState m) -> m [(Key, Key)]
values xs0 = st (go xs0) where
  go :: Order s -> ST s [(Key, Key)]
  go this
    | isNil this = return []
    | otherwise  = do
        n <- get next this
        (:) <$> value this <*> go n
{-# INLINE values #-}
