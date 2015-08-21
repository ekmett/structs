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
module Data.Struct where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive
import GHC.Exts
import GHC.ST

--------------------------------------------------------------------------------
-- * Unboxed Structures
--------------------------------------------------------------------------------

st :: PrimMonad m => ST (PrimState m) a -> m a
st (ST f) = primitive f
{-# INLINE[0] st #-}

{-# RULES "st/id" st = id #-}

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

alloc :: (PrimMonad m, Struct t) => Int -> m (t (PrimState m))
alloc (I# n#) = primitive $ \s -> case newSmallArray# n# undefined s of (# s', b #) -> (# s', construct b #)

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
  Slot gxy _ # Slot gyz syz = Slot
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
  Slot gxy _ # Field gyz syz = Field
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
