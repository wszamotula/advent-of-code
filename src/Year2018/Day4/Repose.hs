module Year2018.Day4.Repose where

import Text.ParserCombinators.ReadP
import Data.Time
import Data.Maybe
import Data.List
import Data.Function
import Control.Applicative
import qualified Data.HashMap.Lazy as Map

type Guard = Int
type Minute = Int

data Action = Start | Sleep | Wake deriving (Show)
data Event = Event {dateTime :: LocalTime, action :: Action, eventGuard :: Guard} deriving (Show)
data Nap = Nap {startMin :: Int, endMin :: Int, napGuard :: Guard} deriving (Show)

main = do
    contents <- readFile "input"
    putStrLn $ show $ strategy2 contents

-- | For the guard that had the most minutes asleep, returns the ID of the guard * the minute they slept the most
strategy1 :: String -> Minute
strategy1 input = let (guard, min) = sleepiestGuardMinute $ napsForGuards 0 $ parseAllLogEvents input
                  in  guard * min

-- | For the the guard that is most frequently asleep on the same minute, return the ID of the guard * the min they slept
strategy2 :: String -> Minute
strategy2 input = let naps = napsForGuards 0 $ parseAllLogEvents input
                      (guard, min) = mostFreqSleepingGuard $ allSleepiestGuardMinutes $ naps
                  in  guard * min

-- | Generate the naps for all the guards documented in those events, use guard ID 0 to start the recursive process
napsForGuards :: Guard -> [Event] -> [Nap]
napsForGuards _ []                 = []
napsForGuards 0 events             = let (start:sortedEvents) = sortBy (compare `on` dateTime) events
                                     in  napsForGuards (eventGuard start) sortedEvents
napsForGuards guard (event:events) = case (action event) of
                                        Start -> napsForGuards (eventGuard event) events
                                        Sleep -> singleNap (event, head events) guard : napsForGuards guard (tail events)

-- | Get a single nap for a gaurd given a pair of sleep/wake events
singleNap :: (Event, Event) -> Guard -> Nap
singleNap (sleepEvent, wakeEvent) guard = Nap (minutes sleepEvent) (minutes wakeEvent - 1) guard
                                       where minutes = todMin . localTimeOfDay . dateTime

-- | Find the sleepiest guard and the minute they slept the most from a list of all naps
sleepiestGuardMinute :: [Nap] -> (Guard, Minute)
sleepiestGuardMinute naps = let napLength nap = endMin nap - startMin nap + 1
                                sleepiestGuardNaps = maximumBy (compare `on` (sum . map napLength)) $ groupGuardNaps naps
                            in  (napGuard $ head sleepiestGuardNaps, fst $ sleepiestMinute sleepiestGuardNaps)

-- | Get the minute each guard slept the most and how much they slept at that minute
allSleepiestGuardMinutes :: [Nap] -> [(Guard, Minute, Int)]
allSleepiestGuardMinutes naps = foldl (\res naps -> guardMin naps : res) [] $ groupGuardNaps naps
                    where guardMin naps = (napGuard $ head naps, fst $ sleepiestMinute naps, snd $ sleepiestMinute naps)

-- | Group a list of naps by guard
groupGuardNaps :: [Nap] -> [[Nap]]
groupGuardNaps naps = groupBy ((==) `on` napGuard) $ sortBy (compare `on` napGuard) naps

-- | Get the guard that slept the most at their most slept minute
mostFreqSleepingGuard :: [(Guard, Minute, Int)] -> (Guard, Minute)
mostFreqSleepingGuard guardNapMins = let (guard, min, _) = maximumBy (compare `on` (\(_, _, amt) -> amt)) guardNapMins
                              in (guard, min)

-- | Find the minute that was most slept from a list of naps and how long they slept at that minute
sleepiestMinute :: [Nap] -> (Minute, Int)
sleepiestMinute naps = let napMinutes nap = [(startMin nap)..(endMin nap)]
                           minMap = foldl (\map nap -> updateNapMinMap map (napMinutes nap)) Map.empty naps
                           updateTopMin res napTime napAmt = if napAmt > snd res then (napTime, napAmt) else res
                       in  Map.foldlWithKey' updateTopMin (0,0) minMap

-- | Update a map from napMin -> napAmt given a list of additional minutes spent sleeping
-- | Maybe could have been done with a flip on partial alter function, then applied with <$> to minutes
updateNapMinMap :: Map.HashMap Minute Int -> [Minute] -> Map.HashMap Minute Int
updateNapMinMap map []         = map
updateNapMinMap map (min:mins) = updateNapMinMap (Map.alter addOrIncrement min map) mins
                            where addOrIncrement Nothing = Just 1
                                  addOrIncrement (Just x) = Just (x + 1)

-- | Parse all of the events from log events seperated by new lines
parseAllLogEvents :: String -> [Event]
parseAllLogEvents = fst . last . readP_to_S (parseLogEvent `sepBy` (char '\n'))

-- | Parse a single log event
parseLogEvent :: ReadP Event
parseLogEvent = do
          dateTime <- parseLogTimes
          char ' '
          guard <- option 0 parseGuard
          actionString <- option "start" (string "falls asleep" <|> string "wakes up")
          return $ Event dateTime (toAction actionString) guard

-- | Convert the string describing log actions into the specific action
toAction :: String -> Action
toAction "start"        = Start
toAction "falls asleep" = Sleep
toAction "wakes up"     = Wake
toAction otherwise      = error "Did not use a valid action"

-- | Parse the string describing which guard started their shift
parseGuard :: ReadP Guard
parseGuard = do
          string "Guard #"
          id <- munch digit
          string " begins shift"
          return $ read id
          where digit = (`elem` ['0'..'9'])

-- | Parse the string describing the time in a log entry
parseLogTimes :: ReadP LocalTime
parseLogTimes = do
          dateTimeStr <- between (char '[') (char ']') (munch timeChars)
          return $ fromJust $ parseTimeM False defaultTimeLocale "%F %R" dateTimeStr
          where timeChars = (`elem` ['-', ':', ' '] ++ ['0'..'9'])
