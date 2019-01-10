module Year2018.Day5.AlchReductionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day5.AlchReduction

tests :: TestTree
tests = testGroup "Day 5 tests" [testUnitsAfterReduction, testUnitsAfterBadUnitReduction]

testUnitsAfterReduction :: TestTree
testUnitsAfterReduction = testCase "Units after reduction for sample input" $
  (unitsAfterReduction "dabAcCaCBAcCcaDA" @?= 10)

testUnitsAfterBadUnitReduction :: TestTree
testUnitsAfterBadUnitReduction = testCase "Units after bad unit reduction for sample input" $
  (unitsAfterBadUnitReduction "dabAcCaCBAcCcaDA" @?= 4)

