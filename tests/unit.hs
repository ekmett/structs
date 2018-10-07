{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
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
  data LinkCut a s = LinkCut
    { path, parent, left, right :: !(LinkCut a s)
    , value, summary :: a
    }
   |]

makeStruct [d|
  data TupleInts a s  = TupleInts
    { tupleLeft, tupleRight :: a
    } 
    |]

-- Create a new tuple of ints
mkTupleInts a b = st (newTupleInts a b)

-- Does not work, Slot is things like left, right
-- setTupleLeft tup val = set tupleLeft tup val


setTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> a -> m ()
setTupleLeft tup val = setField tupleLeft tup val


getTupleLeft :: PrimMonad m => TupleInts a (PrimState m) -> m a
getTupleLeft tup = getField tupleLeft tup



main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  ]


unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , testCase "create and get value from tuple" $ 
      runST $ do
        c <- mkTupleInts 10 20
        val <- getTupleLeft  c
        return (val @?= 10)

  ]
