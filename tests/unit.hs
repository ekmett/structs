{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Data.Struct
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

-- mkTupleInts a b = st (newTupleInts Nil Nil a b)

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
  ]
