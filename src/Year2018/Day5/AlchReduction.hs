module Year2018.Day5.AlchReduction where

import Data.Char
import Data.List

type Polymer = String

main = do
    contents <- readFile "input"
    putStrLn $ show $ unitsAfterBadUnitReduction contents

-- | Determine how many units remain in a polymer after all reactions are done
unitsAfterReduction :: Polymer -> Int
unitsAfterReduction = length . fullReaction

-- | Determine how many units remain in a polymer after removing the unit that prevents the most reactions
unitsAfterBadUnitReduction :: Polymer -> Int
unitsAfterBadUnitReduction = minimum . map length . map fullReaction . filteredPolymers

-- | Generate a list of all polymers with units a-z removed respectively
filteredPolymers :: Polymer -> [Polymer]
filteredPolymers polymer = foldl' (\res unit -> (removeUnit unit polymer):res) [] ['a'..'z']
                           where removeUnit u = filter (\c -> toUpper c /= toUpper u)

-- | Remove all units that react in a polymer until there are no more reactions left
-- | Runs pretty slow, could probably use a faster data structure than String to do this calculation
fullReaction :: Polymer -> Polymer
fullReaction polymer = let newPolymer = singleReaction polymer
                       in  if newPolymer == polymer
                            then polymer
                            else fullReaction newPolymer

-- | Make a single pass through a polymer removing units that react
singleReaction :: Polymer -> Polymer
singleReaction [] = []
singleReaction (c:[]) = [c]
singleReaction (c1:c2:[]) = if doReact c1 c2
                              then []
                              else [c1, c2]
singleReaction (c1:c2:c3:cs) = if doReact c1 c2
                                then singleReaction (c3:cs)
                                else if doReact c2 c3
                                      then singleReaction (c1:cs)
                                      else c1 : c2 : singleReaction (c3:cs)

-- | Check is two characters react with one another
doReact :: Char -> Char -> Bool
doReact c1 c2 = areSameLetter && areUpperAndLower
                where areUpperAndLower = (isUpper c1 && isLower c2) || (isLower c1 && isUpper c2)
                      areSameLetter = toUpper c1 == toUpper c2