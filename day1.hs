import Data.List
import qualified Data.Text as T
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Function

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

numWords = M.fromList
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]

-- Start working down here
part1 = sum . map (read . (\x -> [head x] ++ [last x])) . map (filter isDigit)

-- check if any prefix of the string up to 5 characters matches one of the numWords
-- or if the string starts with a digit. return the strating digit or the corresponding number
-- of the word
parseNum :: String -> Maybe Int
parseNum s = case find (\x -> isPrefixOf x s) (M.keys numWords) of
  Just x -> pure $ numWords M.! x
  Nothing -> if isDigit (head s) then pure $ read [head s] else Nothing

-- now do the same thing but backwards, starting from 
-- parseNum' :: String -> Maybe Int

-- For this function, do parseNum continually forward until the first Just
-- parseNums :: String -> Int
parseNums = head . mapMaybe parseNum . tails
parseNums2 = head . mapMaybe parseNum . tail . reverse . tails

-- part 2 is doing parseNums and parseNums2 on each line and summing the results
part2 = sum . map (\x -> 10 * parseNums x + parseNums2 x)

main = do
  let dayNumber = 1 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let inp' = init (splitOn "\n" inp)
  print (part1 inp')
  print (part2 inp')
