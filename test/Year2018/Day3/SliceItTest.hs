module Year2018.Day3.SliceItTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day3.SliceIt

tests :: TestTree
tests = testGroup "Day 2 tests" [testOverlappedClaims, testClaimWithoutOverlap]

testOverlappedClaims :: TestTree
testOverlappedClaims = testCase "Overlapped claims for sample input" $
  (overlappedSpace "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" @?= 4)

testClaimWithoutOverlap :: TestTree
testClaimWithoutOverlap = testCase "Claim without overlap for sample input" $
  (claimWithoutOverlap "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" @?= 3)