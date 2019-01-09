import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day4.ReposeTest as Day4

main :: IO ()
main = defaultMain $ testGroup "tests" [Day4.tests]

