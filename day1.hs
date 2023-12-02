{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.Char
import Data.List
import Data.Maybe

numWords :: [(String, Int)]
numWords =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

-- Start working down here
part1 :: [[Char]] -> Int
part1 = sum . map ((\x -> 10 * digitToInt (head x) + digitToInt (last x)) . filter isDigit)

-- check if any prefix of the string up to 5 characters matches one of the numWords
-- or if the string starts with a digit. return the strating digit or the corresponding number
-- of the word
parseNum :: String -> Maybe Int
parseNum s@(c : _) = case find ((`isPrefixOf` s) . fst) numWords of
  Just (_, x) -> pure x
  Nothing -> if isDigit c then pure $ ord c - ord '0' else Nothing
parseNum _ = error "not possible"

parseNums :: [Char] -> Int
parseNums = head . mapMaybe parseNum . tails

parseNums2 :: [Char] -> Int
parseNums2 = head . mapMaybe parseNum . tail . reverse . tails

part2 :: [[Char]] -> Int
part2 = sum . map (\x -> 10 * parseNums x + parseNums2 x)

main :: IO ()
main = do
  let dayNumber = 1 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let inp' = lines inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
