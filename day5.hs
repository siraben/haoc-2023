{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes  -Wall #-}

import Data.Foldable
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ["seeds: 79 14 55 13","seed-to-soil map:\n50 98 2\n52 50 48","soil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15","fertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4","water-to-light map:\n88 18 7\n18 25 70","light-to-temperature map:\n45 77 23\n81 45 19\n68 64 13","temperature-to-humidity map:\n0 69 1\n1 0 69","humidity-to-location map:\n60 56 37\n56 93 4\n"]

-- Consider again the example seed-to-soil map:
-- 50 98 2
-- 52 50 48
-- The first line has a destination range start of 50, a source range start of 98, and a range length of 2

-- (destination range start, source range start, range length)
type RangeMap = (Int, Int, Int)

mkRangeMap :: [Int] -> RangeMap
mkRangeMap [a, b, c] = (a, b, c)

-- The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.

-- The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.

-- Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.

-- function to use range map
-- takes a range map and a seed number
-- returns the soil number
rangeMap :: RangeMap -> Int -> Int
rangeMap (d, s, l) n
  | s <= n && n < s + l = d + (n - s)
  | otherwise = n

mapNumber :: Int -> [(Int, Int, Int)] -> Int
mapNumber n m =
  case filter (\(_, s, l) -> s <= n && n < s + l) m of
    [] -> n
    ((d, ss, _) : _) -> d + (n - ss)

findLocation :: Int -> [[(Int, Int, Int)]] -> Int
findLocation = foldl' mapNumber

-- Function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

mapRange :: (Int, Int) -> [(Int, Int, Int)] -> (Int, Int)
mapRange (start, length) mapping = (s, e - s)
  where
    end = start + length
    (s, e) = foldl' go (start, end) mapping
    go (s, e) (d, ss, l) =
      let end = ss + l
          s' = if ss <= start && start < end then d + (start - ss) else s
          e' = if ss < end && end <= end then d + (end - ss) else e
       in (s', e')

findLowestLocation :: [(Int, Int)] -> [[(Int, Int, Int)]] -> Int
findLowestLocation seedRanges maps = go seedRanges (maxBound :: Int)
  where
    go [] lowestLocation = lowestLocation
    go ((start, length) : rest) lowestLocation =
      let s = fst $ foldl' mapRange (start, length) maps
       in go rest (min lowestLocation s)

main = do
  let dayNumber = 5 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  (s : r') <- map (map words . lines) . splitOn "\n\n" <$> readFile dayFilename
  let r = map (map (mkRangeMap . map read) . drop 1) r'
  let seeds = map read (tail $ head s) :: [Int]
  let seedRanges = (\[a, b] -> (a, b)) <$> chunksOf 2 seeds
  -- print seeds
  -- print seedRanges
  -- print r
  print (minimum (map (`findLocation` r) seeds))

  -- why this doesn't work???
  print (findLowestLocation seedRanges r)
  print (findLocation (findLowestLocation seedRanges r) r)

-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]
