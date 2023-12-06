{-# OPTIONS_GHC -Wall #-}

import Control.Arrow
import Criterion.Main
import Data.Foldable

-- Start working down here
part1 :: [(Int, Int)] -> Int
part1 = totalWaysToWin

part2 :: (Int, Int) -> Int
part2 = uncurry waysToWin

waysToWin :: Int -> Int -> Int
waysToWin t d
  | d <= 0 = 0
  | otherwise = floor (max root1 root2) - ceiling (min root1 root2) + 1
  where
    a = -1
    b = fromIntegral t :: Double
    c = fromIntegral (-d) :: Double
    discr = b * b - 4 * a * c
    root1 = (-b - sqrt discr) / (2 * a)
    root2 = (-b + sqrt discr) / (2 * a)

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
