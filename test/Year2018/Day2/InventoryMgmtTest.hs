module Year2018.Day2.InventoryMgmtTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day2.InventoryMgmt

tests :: TestTree
tests = testGroup "Day 2 tests" [testCheckSum, testCommonLetters]

testCheckSum :: TestTree
testCheckSum = testCaseSteps "Checksum for sample input" $ \step -> do
  step "First input"
  (checksum "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab" @?= 12)

testCommonLetters :: TestTree
testCommonLetters = testCaseSteps "Common letters for sample input" $ \step -> do
  step "First input"
  (commonLetters "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz" @?= "fgij")