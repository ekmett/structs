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
module Struct where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.Word
import Data.Primitive
import GHC.Exts
import GHC.Prim
import GHC.ST

--------------------------------------------------------------------------------
-- * Unboxed Structures
--------------------------------------------------------------------------------

st :: PrimMonad m => ST (PrimState m) a -> m a
st (ST f) = primitive f
{-# INLINE[0] st #-}

{-# RULES "st/id" st = id #-}

midBound :: Key
midBound = unsafeShiftR maxBound 1

class Struct t where
  destruct  :: t s -> SmallMutableArray# s Any
  construct :: SmallMutableArray# s Any -> t s
  -- TODO: object :: Coercion t Object requires unsafe tricks to set up, but lets me move things out of executed core and into a `cast`

eqStruct :: Struct t => t s -> t s -> Bool
eqStruct x y = isTrue# (destruct x `sameSmallMutableArray#` destruct y)
{-# INLINE eqStruct #-}

data Object s = Object { runObject :: SmallMutableArray# s Any }

instance Eq (Object s) where
  (==) = eqStruct

instance Struct Object where
  construct = Object
  destruct = runObject

pattern Struct :: () => Struct t => SmallMutableArray# s Any -> t s
pattern Struct x <- (destruct -> x) where
  Struct x = construct x

--------------------------------------------------------------------------------
-- * Tony Hoare's billion dollar mistake
--------------------------------------------------------------------------------

nil :: Object s
nil = runST $ primitive $ \s -> case unsafeCoerce# newSmallArray# 0# undefined s of (# s', u #) -> (# s', Object u #)
{-# NOINLINE nil #-}

isNil :: Struct t => t s -> Bool
isNil t = case nil of
  Object n -> isTrue# (sameSmallMutableArray# (destruct t) n)
{-# INLINE isNil #-}

pattern Nil :: () => Struct t => t s
pattern Nil <- (isNil -> True) where
  Nil = case nil of
    Object n -> construct n

--------------------------------------------------------------------------------
-- * Faking SmallMutableArrayArray#s
--------------------------------------------------------------------------------

writeSmallMutableArraySmallArray# :: SmallMutableArray# s Any -> Int# -> SmallMutableArray# s Any -> State# s -> State# s
writeSmallMutableArraySmallArray# m i a s = unsafeCoerce# writeSmallArray# m i a s
{-# INLINE writeSmallMutableArraySmallArray# #-}

readSmallMutableArraySmallArray# :: SmallMutableArray# s Any -> Int# -> State# s -> (# State# s, SmallMutableArray# s Any #)
readSmallMutableArraySmallArray# m i s = unsafeCoerce# readSmallArray# m i s
{-# INLINE readSmallMutableArraySmallArray# #-}

writeMutableByteArraySmallArray# :: SmallMutableArray# s Any -> Int# -> MutableByteArray# s -> State# s -> State# s
writeMutableByteArraySmallArray# m i a s = unsafeCoerce# writeSmallArray# m i a s
{-# INLINE writeMutableByteArraySmallArray# #-}

readMutableByteArraySmallArray# :: SmallMutableArray# s Any -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
readMutableByteArraySmallArray# m i s = unsafeCoerce# readSmallArray# m i s
{-# INLINE readMutableByteArraySmallArray# #-}

--------------------------------------------------------------------------------
-- * Field Accessors
--------------------------------------------------------------------------------

data Slot x y = Slot 
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, SmallMutableArray# s Any #))
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> State# s)

class Precomposable t where
  ( # ) :: Slot x y -> t y z -> t x z

instance Precomposable Slot where
  Slot gxy sxy # Slot gyz syz = Slot
    (\x s -> case gxy x s of (# s', y #) -> gyz y s')
    (\x z s -> case gxy x s of (# s', y #) -> syz y z s')

slot :: Int# -> Slot s t
slot i = Slot (\m s -> readSmallMutableArraySmallArray# m i s)
              (\m a s -> writeSmallMutableArraySmallArray# m i a s)

get :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> m (y (PrimState m))
get (Slot go _) x = primitive $ \s -> case go (destruct x) s of
   (# s', y #) -> (# s', construct y #)
{-# INLINE get #-}

set :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> y (PrimState m) -> m ()
set (Slot _ go) x y = primitive_ (go (destruct x) (destruct y))
{-# INLINE set #-}

data Field x a = Field
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, a #))
  (forall s. SmallMutableArray# s Any -> a -> State# s -> State# s)

instance Precomposable Field where
  Slot gxy sxy # Field gyz syz = Field
    (\x s -> case gxy x s of (# s', y #) -> gyz y s')
    (\x z s -> case gxy x s of (# s', y #) -> syz y z s')

field :: Int# -> Field s a
field i = Field
  (\m s -> unsafeCoerce# readSmallArray# m i s)
  (\m a s -> unsafeCoerce# writeSmallArray# m i a s)
{-# INLINE field #-}

unboxedField :: Prim a => Int# -> Int# -> Field s a
unboxedField i j = Field 
  (\m s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> readByteArray# mba j s')
  (\m a s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> writeByteArray# mba j a s')
{-# INLINE unboxedField #-}

getField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> m a
getField (Field go _) x = primitive (go (destruct x))
{-# INLINE getField #-}

setField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> a -> m ()
setField (Field _ go) x y = primitive_ (go (destruct x) y)
{-# INLINE setField #-}

--------------------------------------------------------------------------------
-- * List Labeling: Maintain n keys each labeled with n^2 bits.
--------------------------------------------------------------------------------

type Key = Word8

class Struct t => Intrusive t

next :: Intrusive t => Slot t t
next = slot 1#
{-# INLINE next #-}

prev :: Intrusive t => Slot t t
prev = slot 2#
{-# INLINE prev #-}

key :: Intrusive t => Field t Key
key = field 0# -- TODO: unboxedField 0# 0#
{-# INLINE key #-}

-- | Logarithmic time list labeling solution 
data Label s = Label { runLabel :: SmallMutableArray# s Any }

instance Eq (Label s) where (==) = eqStruct

instance Struct Label where
  construct = Label
  destruct = runLabel

instance Intrusive Label

-- | construct an explicit upper structure
makeLabel :: PrimMonad m => Key -> Label (PrimState m) -> Label (PrimState m) -> m (Label (PrimState m))
makeLabel a p n = st $ do
  this <- primitive $ \s -> case newSmallArray# 3# undefined s of (# s', b #) -> (# s', Label b #)
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
  -- growRight this v0 n 1
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
      if | maxBound - vp < j*j -> growLeft p j'
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

--------------------------------------------------------------------------------
-- Order Maintenance
--------------------------------------------------------------------------------

data Order s = Order { runOrder :: SmallMutableArray# s Any }

parent :: Slot Order Label
parent = slot 3#
{-# INLINE parent #-}

instance Eq (Order s) where (==) = eqStruct

instance Struct Order where
  construct = Order
  {-# INLINE construct #-}
  destruct = runOrder
  {-# INLINE destruct #-}

instance Intrusive Order

makeOrder :: PrimMonad m => Label (PrimState m) -> Key -> Order (PrimState m) -> Order (PrimState m) -> m (Order (PrimState m))
makeOrder p a l r = st $ do
  this <- primitive $ \s -> case newSmallArray# 4# undefined s of (# s', b #) -> (# s', Order b #)
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
