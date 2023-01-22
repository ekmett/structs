{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015-2017 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Struct.Internal where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive
import Data.Coerce
import GHC.Exts

-- $setup
-- >>> import Control.Monad.Primitive

#ifdef HLINT
{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
#endif

data NullPointerException = NullPointerException deriving (Show, Exception)

-- | A 'Dict' reifies an instance of the constraint @p@ into a value.
data Dict p where
  Dict :: p => Dict p

-- | Run an ST calculation inside of a PrimMonad. This lets us avoid dispatching everything through the 'PrimMonad' dictionary.
st :: PrimMonad m => ST (PrimState m) a -> m a
st = primToPrim
{-# INLINE[0] st #-}

-- | An instance for 'Struct' @t@ is a witness to the machine-level
--   equivalence of @t@ and @Object@.
class Struct t where
  struct :: Dict (Coercible (t s) (Object s))
#ifndef HLINT
  default struct :: Coercible (t s) (Object s) => Dict (Coercible (t s) (Object s))
#endif
  struct = Dict
  {-# MINIMAL #-}

data Object s = Object { runObject :: SmallMutableArray# s Any }

instance Struct Object

coerceF :: Dict (Coercible a b) -> a -> b
coerceF Dict = coerce
{-# INLINE coerceF #-}

coerceB :: Dict (Coercible a b) -> b -> a
coerceB Dict = coerce
{-# INLINE coerceB #-}

destruct :: Struct t => t s -> SmallMutableArray# s Any
destruct = \x -> runObject (coerceF struct x)
{-# INLINE destruct #-}

construct :: Struct t => SmallMutableArray# s Any -> t s
construct = \x -> coerceB struct (Object x)
{-# INLINE construct #-}

unsafeCoerceStruct :: (Struct x, Struct y) => x s -> y s
unsafeCoerceStruct x = construct (destruct x)

eqStruct :: Struct t => t s -> t s -> Bool
eqStruct = \x y -> isTrue# (destruct x `sameSmallMutableArray#` destruct y)
{-# INLINE eqStruct #-}

instance Eq (Object s) where
  (==) = eqStruct

#ifndef HLINT
pattern Struct :: Struct t => () => SmallMutableArray# s Any -> t s
pattern Struct x <- (destruct -> x) where
  Struct x = construct x
#endif

-- | Allocate a structure made out of `n` slots. Initialize the structure before proceeding!
alloc :: (PrimMonad m, Struct t) => Int -> m (t (PrimState m))
alloc (I# n#) = primitive $ \s -> case newSmallArray# n# undefined s of (# s', b #) -> (# s', construct b #)

--------------------------------------------------------------------------------
-- * Tony Hoare's billion dollar mistake
--------------------------------------------------------------------------------

-- | Box is designed to mirror object's single field but using the 'Null' type
-- instead of a mutable array. This hack relies on GHC reusing the same 'Null'
-- data constructor for all occurrences. Box's field must not be strict to
-- prevent the compiler from making assumptions about its contents.
data Box = Box Null
data Null = Null

-- | Predicate to check if a struct is 'Nil'.
--
-- >>> isNil (Nil :: Object (PrimState IO))
-- True
-- >>> o <- alloc 1 :: IO (Object (PrimState IO))
-- >>> isNil o
-- False
isNil :: Struct t => t s -> Bool
isNil t = isTrue# (
#if MIN_VERSION_base(4,17,0)
  -- In base-4.17.0.0 or later, reallyUnsafePtrEquality# is levity polymorphic
  -- and heterogeneous, so we can directly invoke it on @destruct t@ (of type
  -- @SmallMutableArray# s Any :: UnliftedType@)) and @Null@ (of type
  -- @Null :: Type@).
  reallyUnsafePtrEquality#
#else
  -- In earlier versions of base, reallyUnsafePtrEquality#'s type is more
  -- restrictive: both arguments must have the same type, and the type of the
  -- arguments must be lifted (i.e., of kind @Type@). To make this work, we use
  -- unsafeCoerce# to coerce both arguments to type @Any :: Type@, which allows
  -- the application of reallyUnsafePtrEquality# to typecheck.
  --
  -- Note that we are coercing from SmallMutableArray#, an unlifted type, to
  -- Any, a lifted type. This is on shaky ground, as GHC only guarantees that
  -- coercing to Any works for lifted types. GHC seemed to tolerate coercing
  -- from SmallMutableArray# to Any for many releases, but this stopped working
  -- in GHC 9.6: see https://gitlab.haskell.org/ghc/ghc/-/issues/22813. Luckily,
  -- we can avoid the issue by using a levity polymorphic version of
  -- reallyUnsafePtrEquality# directly, without any intermediate coercions to
  -- Any.
  unsafeCoerce# reallyUnsafePtrEquality#
#endif
    (destruct t) Null)
{-# INLINE isNil #-}

#ifndef HLINT
-- | Truly imperative.
pattern Nil :: Struct t => () => t s
pattern Nil <- (isNil -> True) where
  Nil = unsafeCoerce# Box Null
#endif

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

casSmallMutableArraySmallArray# :: SmallMutableArray# s Any -> Int# -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #)
casSmallMutableArraySmallArray# m i o n s = unsafeCoerce# casSmallArray# m i o n s
{-# INLINE casSmallMutableArraySmallArray# #-}

--------------------------------------------------------------------------------
-- * Field Accessors
--------------------------------------------------------------------------------

-- | A 'Slot' is a reference to another unboxed mutable object.
data Slot x y = Slot
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, SmallMutableArray# s Any #))
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> State# s)
  (forall s. SmallMutableArray# s Any -> SmallMutableArray# s Any -> SmallMutableArray# s Any -> State# s -> (# State# s, Int#, SmallMutableArray# s Any #))

-- | We can compose slots to get a nested slot or field accessor
class Precomposable t where
  ( # ) :: Slot x y -> t y z -> t x z

instance Precomposable Slot where
  Slot gxy _ _ # Slot gyz syz cyz = Slot
    (\x s -> case gxy x s of (# s', y #) -> gyz y s')
    (\x z s -> case gxy x s of (# s', y #) -> syz y z s')
    (\x o n s -> case gxy x s of (# s', y #) -> cyz y o n s')

-- | The 'Slot' at the given position in a 'Struct'
slot :: Int {- ^ slot -} -> Slot s t
slot (I# i) = Slot
  (\m s -> readSmallMutableArraySmallArray# m i s)
  (\m a s -> writeSmallMutableArraySmallArray# m i a s)
  (\m o n s -> casSmallMutableArraySmallArray# m i o n s)

-- | Get the value from a 'Slot'
get :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> m (y (PrimState m))
get (Slot go _ _) = \x -> primitive $ \s -> case go (destruct x) s of
                                            (# s', y #) -> (# s', construct y #)
{-# INLINE get #-}

-- | Set the value of a 'Slot'
set :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> y (PrimState m) -> m ()
set (Slot _ go _) = \x y -> primitive_ (go (destruct x) (destruct y))
{-# INLINE set #-}

-- | Compare-and-swap the value of the slot. Takes the expected old value, the new value and returns if it succeeded and the value found.
cas :: (PrimMonad m, Struct x, Struct y) => Slot x y -> x (PrimState m) -> y (PrimState m) -> y (PrimState m) -> m (Bool, y (PrimState m))
cas (Slot _ _ go) = \m o n -> primitive $ \s -> case go (destruct m) (destruct o) (destruct n) s of
  (# s', i, r #) -> (# s', (tagToEnum# i :: Bool, construct r) #)

-- | A 'Field' is a reference from a struct to a normal Haskell data type.
data Field x a = Field
  (forall s. SmallMutableArray# s Any -> State# s -> (# State# s, a #)) -- get
  (forall s. SmallMutableArray# s Any -> a -> State# s -> State# s) -- set

instance Precomposable Field where
  Slot gxy _ _ # Field gyz syz = Field
    (\x s -> case gxy x s of (# s', y #) -> gyz y s')
    (\x z s -> case gxy x s of (# s', y #) -> syz y z s')

-- | Store the reference to the Haskell data type in a normal field
field :: Int {- ^ slot -} -> Field s a
field (I# i) = Field
  (\m s -> unsafeCoerce# readSmallArray# m i s)
  (\m a s -> unsafeCoerce# writeSmallArray# m i a s)
{-# INLINE field #-}

-- | Store the reference in the nth slot in the nth argument, treated as a MutableByteArray
unboxedField :: Prim a => Int {- ^ slot -} -> Int {- ^ argument -} -> Field s a
unboxedField (I# i) (I# j) = Field
  (\m s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> readByteArray# mba j s')
  (\m a s -> case readMutableByteArraySmallArray# m i s of
     (# s', mba #) -> writeByteArray# mba j a s')
{-# INLINE unboxedField #-}

-- | Initialized the mutable array used by 'unboxedField'. Returns the array
-- after storing it in the struct to help with initialization.
initializeUnboxedField ::
  (PrimMonad m, Struct x) =>
  Int             {- ^ slot     -} ->
  Int             {- ^ elements -} ->
  Int             {- ^ element size -} ->
  x (PrimState m) {- ^ struct   -} ->
  m (MutableByteArray (PrimState m))
initializeUnboxedField (I# i) (I# n) (I# z) m =
  primitive $ \s ->
    case newByteArray# (n *# z) s of
      (# s1, mba #) ->
        (# writeMutableByteArraySmallArray# (destruct m) i mba s1, MutableByteArray mba #)
{-# INLINE initializeUnboxedField #-}

-- | Get the value of a field in a struct
getField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> m a
getField (Field go _) = \x -> primitive (go (destruct x))
{-# INLINE getField #-}

-- | Set the value of a field in a struct
setField :: (PrimMonad m, Struct x) => Field x a -> x (PrimState m) -> a -> m ()
setField (Field _ go) = \x y -> primitive_ (go (destruct x) y)
{-# INLINE setField #-}


--------------------------------------------------------------------------------
-- * Modifiers
--------------------------------------------------------------------------------

modifyField :: (Struct x, PrimMonad m) => Field x a -> x (PrimState m) -> (a -> a) -> m ()
modifyField s = \o f -> st (setField s o . f =<< getField s o)
{-# INLINE modifyField #-}

modifyField' :: (Struct x, PrimMonad m) => Field x a -> x (PrimState m) -> (a -> a) -> m ()
modifyField' s = \o f -> st (setField s o =<< (\x -> return $! f x) =<< getField s o)
{-# INLINE modifyField' #-}
