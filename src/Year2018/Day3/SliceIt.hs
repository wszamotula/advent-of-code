module Year2018.Day3.SliceIt where

import Text.ParserCombinators.ReadP
import qualified Data.HashMap.Lazy as Map
import qualified Control.Monad.State.Lazy as State
import Data.Maybe
import Data.List

data Claim = Claim {claimId :: Int, leftOffset :: Int, topOffset :: Int, width :: Int, height :: Int} deriving (Show, Eq)

type Fabric = Map.HashMap (Int, Int) Int
type FabricState = (Fabric, Int)

main :: IO ()
main = do
    contents <- readFile "input"
    putStrLn $ show $ claimWithoutOverlap contents

-- | Get the claim that is not overlapping any other claims
claimWithoutOverlap :: String -> Int
claimWithoutOverlap claimString = let claims = parseAllClaims claimString
                                      finalFabric = fst $ State.execState (addAllClaims claims) (Map.empty, 0)
                                  in  claimId $ findClaim claims finalFabric

-- | Find the claim that has no overlaps on the fabric
findClaim :: [Claim] -> Fabric -> Claim
findClaim [] _ = Claim 0 0 0 0 0
findClaim (c:cs) fabric = if claimHasNoOverlaps
                            then c
                            else findClaim cs fabric
                          where points = pointsInClaim c
                                claimHasNoOverlaps = all (\p -> Map.lookup p fabric == Just 1) points

-- | Get the number of overlapping spaces for claims
overlappedSpace :: String -> Int
overlappedSpace claimString = State.evalState (addAllClaims $ parseAllClaims claimString) (Map.empty, 0)

-- | Add a list of claims to the fabric state and return the total number of overlapping spaces
addAllClaims :: [Claim] -> State.State FabricState Int
addAllClaims [] = do
                (_, overlaps) <- State.get
                return overlaps
addAllClaims (c:cs) = do
                (fabric, overlaps) <- State.get
                State.put (updateFabric points fabric, overlaps + newOverlapsInFabric points fabric)
                addAllClaims cs
                where points = pointsInClaim c

-- | Gets updated fabric with number of claims on each points
updateFabric :: [(Int, Int)] -> Fabric -> Fabric
updateFabric [] fabric = fabric
updateFabric (p:ps) fabric = updateFabric ps (Map.alter addOrIncrement p fabric)
                             where addOrIncrement Nothing = Just 1
                                   addOrIncrement (Just x) = Just (x + 1)

-- | Get the number of new points that overlap existing claims on the fabric
newOverlapsInFabric :: [(Int, Int)] -> Fabric -> Int
newOverlapsInFabric [] _ = 0
newOverlapsInFabric (p:ps) fabric = if isClaimedOnce
                                      then 1 + newOverlapsInFabric ps fabric
                                      else newOverlapsInFabric ps fabric
                                    where isClaimedOnce = case Map.lookup p fabric of
                                                             Nothing -> False
                                                             Just 1 -> True
                                                             _ -> False

-- | Get a list of all the points on fabric in a claim
pointsInClaim :: Claim -> [(Int, Int)]
pointsInClaim claim = let xPoints = [(leftOffset claim + 1)..(leftOffset claim + width claim)]
                          yPoints = [(topOffset claim + 1)..(topOffset claim + height claim)]
                      in  (\x y -> (x, y)) <$> xPoints <*> yPoints

-- | Parse a list of claims on separate lines
parseAllClaims :: String -> [Claim]
parseAllClaims = fst . last . readP_to_S (parseClaim `sepBy` (char '\n'))

-- | Parse a string representation of a claim
parseClaim :: ReadP Claim
parseClaim = do
        char '#'
        claimId <- munch digits
        string " @ "
        leftOff:topOff:[] <- (munch digits) `sepBy` (char ',')
        string ": "
        width:height:[] <- (munch digits) `sepBy` (char 'x')
        return $ Claim (read claimId) (read leftOff) (read topOff) (read width) (read height)
        where digits = (`elem` ['0'..'9'])