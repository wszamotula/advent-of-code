import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day5.AlchReductionTest as Day5

main :: IO ()
main = defaultMain $ testGroup "tests" [Day5.tests]

