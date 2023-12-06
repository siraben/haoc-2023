{-# OPTIONS_GHC -Wall #-}

import Control.Arrow
import Criterion.Main
import Data.Foldable

-- Start working down here
part1 :: [(Int, Int)] -> Int
part1 = totalWaysToWin

part2 :: (Int, Int) -> Int
part2 = uncurry waysToWin

-- go through each hold time, calc distance, keep the ones that beat record
waysToWin :: Int -> Int -> Int
waysToWin t d = length $ filter (> d) [hold * (t - hold) | hold <- [0 .. (t - 1)]]

totalWaysToWin :: [(Int, Int)] -> Int
totalWaysToWin = product . map (uncurry waysToWin)

main :: IO ()
main = do
  let races = [(61, 643), (70, 1184), (90, 1362), (66, 1041)] :: [(Int, Int)]
  let (t, d) = (read *** read) (foldl' (\(x, y) (a, b) -> (x ++ show a, y ++ show b)) ("", "") races)
  print (part1 races)
  print (part2 (t, d))
  defaultMain
    [ bgroup
        "day6"
        [ bench "part1" $ whnf part1 races,
          bench "part2" $ whnf part2 (t, d)
        ]
    ]
