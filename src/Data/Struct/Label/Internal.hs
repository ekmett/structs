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
module Data.Struct.Label.Internal where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Struct
import Data.Word

--------------------------------------------------------------------------------
-- * List Labeling: Maintain n keys each labeled with n^2 bits.
--------------------------------------------------------------------------------

type Key = Word8 -- small universe for testing

midBound :: Key
midBound = unsafeShiftR maxBound 1

class Struct t => Intrusive t

next :: Intrusive t => Slot t t
next = slot 1
{-# INLINE next #-}

prev :: Intrusive t => Slot t t
prev = slot 2
{-# INLINE prev #-}

key :: Intrusive t => Field t Key
key = field 0
{-# INLINE key #-}

-- | Logarithmic time list labeling solution 
newtype Label s = Label (Object s)

instance Eq (Label s) where (==) = eqStruct

instance Struct Label where
  struct _ = Dict

instance Intrusive Label

-- | construct an explicit upper structure
makeLabel :: PrimMonad m => Key -> Label (PrimState m) -> Label (PrimState m) -> m (Label (PrimState m))
makeLabel a p n = st $ do
  this <- alloc 3
  setField key this a
  set next this n
  set prev this p
  return this
{-# INLINE makeLabel #-}

-- | O(1). Create a new labeling structure. Labels from different list labeling structures are incomparable.
newLabel :: PrimMonad m => m (Label (PrimState m))
newLabel = makeLabel midBound Nil Nil
{-# INLINE newLabel #-}

-- | O(1) -- TODO: redistribute?
deleteLabel :: PrimMonad m => Label (PrimState m) -> m ()
deleteLabel this = st $ unless (isNil this) $ do
  p <- get prev this
  n <- get next this
  unless (isNil p) $ do
    set next p n
    set prev this Nil
  unless (isNil n) $ do
    set prev n p
    set next this Nil
{-# INLINE deleteLabel #-}

-- | O(log n) amortized. Insert a new label after the current label
insertAfterLabel :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
insertAfterLabel this = st $ do
  v0 <- getField key this
  n <- get next this
  v1 <- if isNil n
        then return maxBound
        else getField key n
  fresh <- makeLabel (v0 + unsafeShiftR (v1 - v0) 1) this n
  set next this fresh
  unless (isNil n) $ set prev n fresh
  unless True $ growRight this v0 n 1
  return fresh
 where
  growRight :: Label s -> Key -> Label s -> Key -> ST s ()
  growRight !n0 !_ Nil !j = growLeft n0 j
  growRight n0 v0 nj j = getField key nj >>= \vj -> if
    | vj-v0 < j*j -> do nj' <- get next nj
                        growRight n0 v0 nj' (j+1)
    | otherwise   -> do n1 <- get next n0
                        balance n1 v0 j j

  growLeft :: Label s -> Key -> ST s ()
  growLeft !c !j = get prev c >>= \p -> if
    | isNil p   -> balance c minBound (div maxBound j) j -- we've found the other extreme
    | otherwise -> do
      vp <- getField key p
      p' <- get prev p
      let !j' = j + 1
      if | maxBound - vp < j*j -> growLeft p' j'
         | otherwise           -> balance c vp (div (maxBound - vp) j') j'

  balance :: Label s -> Key -> Key -> Key -> ST s ()
  balance !_ !_ !_ 0 = return ()
  balance Nil _ _ _ = return ()
  balance c v dv j = do
    let !v' = v + dv
    setField key c v'
    n <- get next c
    balance n v' dv (j-1)
{-# INLINE insertAfterLabel #-}

-- | O(n). Get the keys of every label from here to the right.
keys :: PrimMonad m => Label (PrimState m) -> m [Key]
keys this = st $ if 
  | isNil this -> return []
  | otherwise -> do
    x <- getField key this
    n <- get next this 
    xs <- keys n
    return (x:xs)
{-# INLINE keys #-}

-- | O(n). Seek all the way to the left of the list of labels
firstLabel :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
firstLabel xs0 = st $ go xs0 where
  go :: Label s -> ST s (Label s)
  go this = get prev this >>= \p -> if
    | isNil p   -> return this
    | otherwise -> firstLabel p
{-# INLINE firstLabel #-}

-- | O(n). Seek all the way to the right of the list of labels.
lastLabel :: PrimMonad m => Label (PrimState m) -> m (Label (PrimState m))
lastLabel xs0 = st $ go xs0 where
  go :: Label s -> ST s (Label s)
  go this = get next this >>= \case
    Nil -> return this
    p -> lastLabel p
{-# INLINE lastLabel #-}


-- | O(1). Split off all labels after the current point.
cutAfterLabel :: PrimMonad m => Label (PrimState m) -> m ()
cutAfterLabel this = st $ do
  n <- get next this
  unless (isNil n) $ do
    set next this Nil
    set prev n Nil
{-# INLINE cutAfterLabel #-}

-- | O(1). Split off all labels before the current point.
cutBeforeLabel :: PrimMonad m => Label (PrimState m) -> m ()
cutBeforeLabel this = st $ do
  p <- get prev this
  unless (isNil p) $ do
    set next p Nil
    set prev this Nil
{-# INLINE cutBeforeLabel #-}

class OrdM t where
  compareM :: PrimMonad m => t (PrimState m) -> t (PrimState m) -> m Ordering

instance OrdM Label where
  compareM i j = compare <$> getField key i <*> getField key j
  {-# INLINE compareM #-}

