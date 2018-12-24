import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day3.SliceItTest as Day3

main :: IO ()
main = defaultMain $ testGroup "tests" [Day3.tests]

