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
module Data.Struct.Internal.Label where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Struct.Internal
import Data.Word

------------------------------------------------------------------------------------
-- * List Labeling: Maintain n keys each labeled with n^2 bits w/ log n update time.
--
-- After about 2^32 elements, this structure will continue to work, but will become
-- unacceptably slow and the asymptotic analysis will become wrong.
------------------------------------------------------------------------------------

type Key = Word64

midBound :: Key
midBound = unsafeShiftR maxBound 1

class Struct t => IsLabel t where
  -- * Members
  key  :: Field t Key
  next, prev :: Slot t t

  -- * Methods
  compareM :: PrimMonad m => t (PrimState m) -> t (PrimState m) -> m Ordering

  -- | O(1). Remove a label
  delete :: PrimMonad m => t (PrimState m) -> m ()

  insertAfter :: PrimMonad m => t (PrimState m) -> m (t (PrimState m))

  -- | O(1). Split off all labels before/after the current point.
  cutBefore, cutAfter :: PrimMonad m => t (PrimState m) -> m ()

  -- | O(n). Retrieve the least/greatest label
  least, greatest :: PrimMonad m => t (PrimState m) -> m (t (PrimState m))

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
newLabel :: PrimMonad m => m (Label (PrimState m))
newLabel = makeLabel midBound Nil Nil
{-# INLINE newLabel #-}

-- | O(1) delete, O(log n) amortized insert
instance IsLabel Label where
  key = field 0
  {-# INLINE key #-}
  next = slot 1
  {-# INLINE next #-}
  prev = slot 2
  {-# INLINE prev #-}
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

  insertAfter this = st $ do
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
    growRight n0 v0 nj j = getField key nj >>= \vj -> if
      | fromIntegral (vj-v0) < j*j -> do
          nj' <- get next nj
          growRight n0 v0 nj' (j+1)
      | otherwise   -> do
          n1 <- get next n0 -- start at the fresh node
          balance n1 v0 (delta (vj-v0) j) j -- it moves over
  
    growLeft :: Label s -> Word64 -> ST s ()
    growLeft !c !j = get prev c >>= \p -> if
      | isNil p   -> balance c 0 (delta maxBound j) j -- full rebuild
      | otherwise -> do
        vp <- getField key p
        p' <- get prev p
        let !j' = j+1
        if | fromIntegral (maxBound - vp) < j'*j' -> growLeft p' j'
           | otherwise -> balance c vp (delta (maxBound-vp) j') j'
  
    balance :: Label s -> Key -> Key -> Word64 -> ST s ()
    balance !_ !_ !_ 0 = return ()
    balance Nil _ _ _ = return () -- error "balanced past the end" -- return ()
    balance c v dv j = do
      let !v' = v + dv
      setField key c v'
      n <- get next c
      balance n v' dv (j-1)
  {-# INLINE insertAfter #-}

  cutAfter this = st $ do
    n <- get next this
    unless (isNil n) $ do
      set next this Nil
      set prev n Nil
  {-# INLINE cutAfter #-}

  cutBefore this = st $ do
    p <- get prev this
    unless (isNil p) $ do
      set next p Nil
      set prev this Nil
  {-# INLINE cutBefore #-}

  least xs0 = st $ go xs0 where
    go :: Label s -> ST s (Label s)
    go this = get prev this >>= \p -> if
      | isNil p   -> return this
      | otherwise -> least p
  {-# INLINE least #-}

  greatest xs0 = st $ go xs0 where
    go :: Label s -> ST s (Label s)
    go this = get next this >>= \case
      Nil -> return this
      p -> greatest p
  {-# INLINE greatest #-}

  compareM i j = compare <$> getField key i <*> getField key j
  {-# INLINE compareM #-}
  
delta :: Key -> Word64 -> Key
delta m j = fromIntegral $ max 1 $ quot (fromIntegral m) (j+1)

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

