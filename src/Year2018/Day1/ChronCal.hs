module Year2018.Day1.ChronCal where

main :: IO ()
main = do
  contents <- readFile "input"
  putStrLn $ show $ calibrateRepeating 0 contents

-- | Apply all of the given frequency changes to a starting frequency and return the result
calibrate :: Int    -- ^ Starting frequency
          -> String -- ^ String describing changes to frequency
          -> Int    -- ^ Frequency after applying all changes
calibrate freq = foldr (\change acc -> change acc) freq . allFreqChanges

-- | Find the first frequency that repeats after applying repeatedly applying frequency changes to starting frequency
calibrateRepeating :: Int     -- ^ Starting frequency
                   -> String  -- ^ String describing repeated changes to frequency
                   -> Int     -- ^ First frequency that repeats
calibrateRepeating freq = findRepeated [freq] . cycle . allFreqChanges

-- | Apply changes to list of previous frequencies until a value is repeated
-- | This is pretty slow, should probably switch the previous frequencies to a map or set
findRepeated :: [Int]           -- ^ Previous frequencies visited
             -> [(Int -> Int)]  -- ^ List of changes to apply to frequencies
             -> Int             -- ^ First frequency that was repeated
findRepeated prev@(x:xs) (change:changes) = if next `elem` prev
                                              then next
                                              else findRepeated (next:prev) changes
                                            where next = change x

-- | Get all of the frequency changing functions from a string describing frequency changes
allFreqChanges :: String -> [(Int -> Int)]
allFreqChanges = foldr (\str acc -> freqChange str : acc) [] . words

-- | Get a single frequency changing function from a string describing one frequency change
freqChange :: String -> (Int -> Int)
freqChange ('+':val) = \x -> x + read val
freqChange ('-':val) = \x -> x - read val