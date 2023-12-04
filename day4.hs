{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List

-- Start working down here
part1 :: [(IntSet, IntSet)] -> Int
part1 = sum . map (f . score)
  where
    f 0 = 0
    f n = 2 ^ (n - 1)

score :: (IntSet, IntSet) -> Int
score (a, b) = IS.size $ IS.intersection a b

-- zipWith (+) but for lists of different lengths
zipAdd :: [Int] -> [Int] -> [Int]
zipAdd l [] = l
zipAdd [] l = l
zipAdd (x : xs) (y : ys) = (x + y) : zipAdd xs ys

part2 :: [(IntSet, IntSet)] -> Int
part2 inp = snd $ foldl' go (replicate (length inp) 1, 0) inp
  where
    go (c : rs, tot) g = (zipAdd (replicate (score g) c) rs, tot + c)
    go _ _ = error "not possible"

process :: String -> (IntSet, IntSet)
process = f . span (/= "|") . drop 2 . words
  where
    f (a, b) = (IS.fromList $ map read a, IS.fromList $ map read (tail b))

main :: IO ()
main = do
  let dayNumber = 4 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map process . lines <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
