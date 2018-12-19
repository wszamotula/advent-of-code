import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day1.ChronCalTest as Day1

main :: IO ()
main = defaultMain $ testGroup "tests" [Day1.tests]