module Year2018.Day1.ChronCalTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day1.ChronCal

tests :: TestTree
tests = testGroup "Day 1 tests" [testExampleCalibration, testExampleCalibrationRepeating]

testExampleCalibration :: TestTree
testExampleCalibration = testCaseSteps "Calibrate with sample inputs" $ \step -> do
  step "first sample input"
  (calibrate 0 "+1\n-2\n+3\n+1" @?= 3)

  step "second sample input"
  (calibrate 1 "+1\n+1\n+1\n" @?= 4)

  step "third sample input"
  (calibrate (-1) "+1\n+1\n-2" @?= -1)

  step "fourth sample input"
  (calibrate 0 "-1\n-2\n-3" @?= -6)

testExampleCalibrationRepeating :: TestTree
testExampleCalibrationRepeating = testCaseSteps "Calibrate rotating with sample inputs" $ \step -> do
  step "first sample input"
  (calibrateRepeating 0 "+1\n-1" @?= 0)

  step "second sample input"
  (calibrateRepeating 0 "+3\n+3\n+4\n-2\n-4" @?= 10)

  step "third sample input"
  (calibrateRepeating 0 "-6\n+3\n+8\n+5\n-6" @?= 5)

  step "fourth sample input"
  (calibrateRepeating 0 "+7\n+7\n-2\n-7\n-4" @?= 14)