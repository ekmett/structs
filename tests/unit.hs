{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Test.Tasty.HUnit

import Data.Ord

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Struct.Internal
import Data.Struct.TH


-- Simple use of makeStruct
makeStruct [d|
  data TupleInts a s  = TupleInts
    { tupleLeft, tupleRight :: a
    }
    |]

-- Create a new tuple of ints
mkTupleInts a b = st (newTupleInts a b)

setTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> a -> m ()
setTupleLeft tup val = setField tupleLeft tup val

getTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> m a
getTupleLeft tup = getField tupleLeft tup


-- Questions on API:
-- How does Nil work

-- makeStruct of a data type with pointers.

makeStruct [d|
  data LinkedList a s  = LinkedList
    { val :: a,
       next :: !(LinkedList a s) }
    |]

-- Make an empty linked list
mkEmptyLinkedList ::  LinkedList a s
mkEmptyLinkedList = Nil

-- Make a linked list node with a value
mkLinkedListNode :: PrimMonad m => a -> m (LinkedList a (PrimState m))
mkLinkedListNode a = newLinkedList a Nil

-- Append a node to a linked list.
appendLinkedList :: PrimMonad m =>
  LinkedList x (PrimState m)
  -> x
  -> m (LinkedList x (PrimState m))
appendLinkedList xs x = do
  isend <- isNil <$> (get next xs)
  if isend
     then do
       nodex <- mkLinkedListNode x
       set next xs nodex
       return xs
      else do
        xs' <- get next xs
        appendLinkedList xs' x

-- Retreive the nth value from the linked list.
nthLinkedList :: PrimMonad m => Int -> LinkedList a (PrimState m) -> m a
nthLinkedList 0 xs = getField val xs
nthLinkedList i xs = get next xs >>= nthLinkedList (i - 1)

-- Convert a haskell list to a linked list
listToLinkedList :: PrimMonad m => [a] -> m (LinkedList a (PrimState m))
listToLinkedList [] = return mkEmptyLinkedList
listToLinkedList (x:xs) = do
  head <- mkLinkedListNode x
  rest <- listToLinkedList xs
  set next head rest

  return head


-- TODO: setup ViewPatterns  to check when something is nil
-- concat xs ys ==  xs := xs ++ ys
concatLinkedList :: PrimMonad m =>
      LinkedList a (PrimState m)
  -> LinkedList a (PrimState m)
  -> m ()
concatLinkedList xs ys =
  if isNil xs
     then error "head of list is undefined"
     else do
       isend <- isNil <$> (get next xs)
       if isend
           then set next xs ys
           else get next xs >>= \xs' -> concatLinkedList xs' ys


-- datatype with UNPACKED
makeStruct [d| data Vec3 s  = Vec3 { x, y, z :: {-# UNPACK #-} !Int  } |]

-- Test bench
-- ==========
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]


-- Return if a list equal to some linked list representation.
listEqLinkedList :: PrimMonad m => Eq a => [a] -> LinkedList a (PrimState m) -> m Bool
listEqLinkedList [] l = return $ isNil l
listEqLinkedList (x:xs) l = do
  xval <- getField val l
  if xval == x
     then do
       l' <- get next l
       listEqLinkedList xs l'
    else return False


qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty @([Int] -> Bool) "list to linked list" $
    \xs -> runST $ do
      lxs <- listToLinkedList xs
      listEqLinkedList xs lxs

  , QC.testProperty @(NonEmptyList Int -> Bool) "Indexing linked lists" $
    \xs -> runST $ do
        lxs <- listToLinkedList (getNonEmpty xs)

        -- TODO: missing Foldable instance for NonEmptyList
        xsAtIx <- sequenceA [nthLinkedList ix lxs | ix <- [0.. length (getNonEmpty xs) - 1]]
        return $ xsAtIx  == getNonEmpty xs

        -- return $ getNonEmpty lxs == xsAtIx

  , QC.testProperty @(NonEmptyList Int -> [Int] -> Bool) "Appending linked lists" $
    \xs ys -> runST $ do
      lxs <- listToLinkedList (getNonEmpty xs)
      lys <- listToLinkedList ys

      -- this mutates lxs
      concatLinkedList lxs lys

      listEqLinkedList ((getNonEmpty xs) ++ ys) lxs
  ]



-- Try out the `Precomposable` system
nextnext :: Slot (LinkedList a) (LinkedList a)
nextnext = next # next

nextnextval :: Field (LinkedList a) a
nextnextval = nextnext # val


unitTests = testGroup "Unit tests"
  [ testCase "create and get value from tuple" $
      runST $ do
        c <- mkTupleInts 10 20
        val <- getTupleLeft  c
        return (val @?= 10)
  , testCase "set and get value from tuple" $ runST $ do
        c <- mkTupleInts 10 20
        setTupleLeft c 30
        val <- getTupleLeft c
        return (val @?= 30)
  , testCase "pull the values out of a linked list using nextnextval" $ runST $ do
      xs <- listToLinkedList [1, 2, 3]
      nnv <- getField nextnextval xs
      return (nnv @?= 3)

  ]
