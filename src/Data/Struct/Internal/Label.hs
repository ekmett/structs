{-# LANGUAGE CPP #-}
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
module Data.Struct.Internal.Label where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Struct.Internal
import Data.Word

#ifdef HLINT
{-# ANN module "HLint: ignore Eta reduce" #-}
#endif

------------------------------------------------------------------------------------
-- * List Labeling: Maintain n keys each labeled with n^2 bits w/ log n update time.
--
-- After about 2^32 elements, this structure will continue to work, but will become
-- unacceptably slow and the asymptotic analysis will become wrong.
------------------------------------------------------------------------------------

type Key = Word64

midBound :: Key
midBound = unsafeShiftR maxBound 1

key :: Field Label Key
key = field 0
{-# INLINE key #-}

next :: Slot Label Label
next = slot 1
{-# INLINE next #-}

prev :: Slot Label Label
prev = slot 2
{-# INLINE prev #-}

-- | Logarithmic time list labeling solution
newtype Label s = Label (Object s)

instance Eq (Label s) where (==) = eqStruct

instance Struct Label where
  struct _ = Dict

-- | Construct an explicit list labeling structure.
--
-- >>> x <- makeLabel 0 Nil Nil
-- >>> isNil x
-- False
-- >>> n <- get next x
-- >>> isNil n
-- True
-- >>> p <- get prev x
-- >>> isNil p
-- True

makeLabel :: PrimMonad m => Key -> Label (PrimState m) -> Label (PrimState m) -> m (Label (PrimState m))
makeLabel a p n = st $ do
  this <- alloc 3
  setField key this a
  set next this n
  set prev this p
  return this
{-# INLINE makeLabel #-}

-- | O(1). Create a new labeling structure. Labels from different list labeling structures are incomparable.
new :: PrimMonad m => m (Label (PrimState m))
new = makeLabel midBound Nil Nil
{-# INLINE new #-}

-- | O(1). Remove a label
delete :: PrimMonad m => Label  (PrimState m) -> m ()
delete this = st $ unless (isNil this) $ do
  p <- get prev this
  n <- get next this
  unless (isNil p) $ do
    set next p n
    set prev this Nil
  unless (isNil n) $ do
    set prev n p
    set next this Nil
{-# INLINE delete #-}

-- | O(log n) amortized. Insert a new label after a given label.
insertAfter :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
insertAfter this = st $ do
  when (isNil this) $ throw NullPointerException
  v0 <- getField key this
  n <- get next this
  v1 <- if isNil n
        then return maxBound
        else getField key n
  fresh <- makeLabel (v0 + unsafeShiftR (v1 - v0) 1) this n
  set next this fresh
  unless (isNil n) $ set prev n fresh
  growRight this v0 n 2
  return fresh
 where
  growRight :: Label s -> Key -> Label s -> Word64 -> ST s ()
  growRight !n0 !_ Nil !j = growLeft n0 j
  growRight n0 v0 nj j = do
    vj <- getField key nj
    if fromIntegral (vj-v0) < j*j
    then do
      nj' <- get next nj
      growRight n0 v0 nj' (j+1)
    else do
      n1 <- get next n0 -- start at the fresh node
      balance n1 v0 (delta (vj-v0) j) j -- it moves over

  growLeft :: Label s -> Word64 -> ST s ()
  growLeft !c !j = do
    p <- get prev c
    if isNil p
    then balance c 0 (delta maxBound j) j -- full rebuild
    else do
      vp <- getField key p
      p' <- get prev p
      let !j' = j+1
      if fromIntegral (maxBound - vp) < j'*j'
      then growLeft p' j'
      else balance c vp (delta (maxBound-vp) j') j'

  balance :: Label s -> Key -> Key -> Word64 -> ST s ()
  balance !_ !_ !_ 0 = return ()
  balance Nil _ _ _ = return () -- error "balanced past the end" -- return ()
  balance c v dv j = do
    let !v' = v + dv
    setField key c v'
    n <- get next c
    balance n v' dv (j-1)
{-# INLINE insertAfter #-}

-- | O(1). Split off all labels after the current label.
cutAfter :: PrimMonad m => Label (PrimState m) -> m ()
cutAfter this = st $ do
  when (isNil this) $ throw NullPointerException
  n <- get next this
  unless (isNil n) $ do
    set next this Nil
    set prev n Nil
{-# INLINE cutAfter #-}

-- | O(1). Split off all labels before the current label.
cutBefore :: PrimMonad m => Label (PrimState m) -> m ()
cutBefore this = st $ do
  when (isNil this) $ throw NullPointerException
  p <- get prev this
  unless (isNil p) $ do
    set next p Nil
    set prev this Nil
{-# INLINE cutBefore #-}

-- | O(n). Retrieve the least label
least :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
least xs0
  | isNil xs0 = throw NullPointerException
  | otherwise = st $ go xs0 where
  go :: Label s -> ST s (Label s)
  go this = do
    p <- get prev this
    if isNil p
    then return this
    else go p
{-# INLINE least #-}

-- | O(n). Retrieve the greatest label
greatest :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
greatest xs0
  | isNil xs0 = throw NullPointerException
  | otherwise = st $ go xs0 where
  go :: Label s -> ST s (Label s)
  go this = do
    n <- get next this
    if isNil n
    then return this
    else go n
{-# INLINE greatest #-}

-- | O(1). Compare two labels for ordering.
compareM :: PrimMonad m => Label (PrimState m) -> Label (PrimState m) -> m Ordering
compareM i j
  | isNil i || isNil j = throw NullPointerException
  | otherwise = compare <$> getField key i <*> getField key j
{-# INLINE compareM #-}

delta :: Key -> Word64 -> Key
delta m j = fromIntegral $ max 1 $ quot (fromIntegral m) (j+1)
{-# INLINE delta #-}

-- | O(1). Extract the current value assignment for this label. Any label mutation, even on other labels in this label structure, may change this answer.
value :: PrimMonad m => Label (PrimState m) -> m Key
value this = getField key this
{-# INLINE value #-}

-- | O(n). Get the keys of every label from here to the right.
keys :: PrimMonad m => Label (PrimState m) -> m [Key]
keys this = st $
  if isNil this
  then return []
  else do
    x <- getField key this
    n <- get next this
    xs <- keys n
    return (x:xs)
{-# INLINE keys #-}
