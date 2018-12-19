module Year2018.Day2.InventoryMgmt where

import Data.List

main :: IO ()
main = do
  contents <- readFile "input"
  putStrLn $ show $ commonLetters contents

-- | Get checksum value for list of boxes by multiplying count of boxes with exactly two of any letter by the count of
-- | boxes with exactly three of any letter
checksum :: String -- ^ List of boxes separated by lines
         -> Int    -- ^ Checksum value
checksum input = let boxes = lines input
                     boxesTwoLetters = filter (exactlyXOfLetter 2) boxes
                     boxesThreeLetters = filter (exactlyXOfLetter 3) boxes
                 in  length boxesTwoLetters * length boxesThreeLetters

-- | Find the two boxes that are only off by one letter and return their matching letters
commonLetters :: String -- ^ List of boxes separated by lines
              -> String -- ^ Matching letters from matching pair
commonLetters input = let boxes = lines input
                          idLength = length (head boxes)
                          offByOne chars = length chars == (idLength - 1)
                          Just result = find offByOne (matchingCharacters <$> boxes <*> boxes)
                      in  result

-- | Check if a box ID has exactly X number of any letter
exactlyXOfLetter :: Int -> String  -> Bool
exactlyXOfLetter count = any (\x -> length x == count) . group . sort

-- | Return matching characters for two box IDs
matchingCharacters :: String -> String -> String
matchingCharacters boxOne boxTwo = map fst $ filter (\pair -> fst pair == snd pair) $ zip boxOne boxTwo