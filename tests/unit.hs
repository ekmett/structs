{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Struct.Internal
import Data.Struct.TH


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
-- Why can't val :: !a?
-- How to create "NULL" LinkedList ? Empty linked list

makeStruct [d|
  data LinkedList a s  = LinkedList
    { val :: (Maybe a),
       next :: !(LinkedList a s) }
    |]

-- Make an empty linked list
mkEmptyLinkedList :: PrimMonad m => m (LinkedList a (PrimState m))
mkEmptyLinkedList = newLinkedList Nothing Nil


-- Make a linked list node with a value
mkLinkedListNode :: PrimMonad m => a -> m (LinkedList a (PrimState m))
mkLinkedListNode a = newLinkedList (Just a) Nil

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

nthLinkedList :: PrimMonad m => Int -> LinkedList a (PrimState m) -> m (Maybe a)
nthLinkedList 0 xs = getField val xs
nthLinkedList i xs = get next xs >>= nthLinkedList (i - 1)

listToLinkedList :: PrimMonad m => [a] -> m (LinkedList a (PrimState m))
listToLinkedList [] = mkEmptyLinkedList 
listToLinkedList (x:xs) = do
  head <- mkLinkedListNode x
  rest <- listToLinkedList xs
  set next head rest

  return head


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty @ ([Int] -> Bool) "list to linked list" $ 
    \xs -> runST $ do
      lxs <- listToLinkedList xs
      vals <- sequenceA [nthLinkedList i lxs | i <- [0..length xs - 1]]
      return $ all (uncurry (==)) (zip (map Just xs) vals)
  ]


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
  , testCase "make a linked list, and check the head has the correct value" $ runST $ do
      head <- mkLinkedListNode 10
      v <- (nthLinkedList 0 head)
      return $ v @?= Just 10
  , testCase "join two linked lists, check that this works" $ runST $ do
      head <- mkLinkedListNode 10
      v <- (nthLinkedList 0 head)
      return $ v @?= Just 10

  ]
