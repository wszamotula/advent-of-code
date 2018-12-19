import Test.Tasty
import Test.Tasty.HUnit

import Year2018.Day1.ChronCalTest as Day1
import Year2018.Day2.InventoryMgmtTest as Day2

main :: IO ()
main = defaultMain $ testGroup "tests" [Day2.tests]