{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

useRule :: Char -> IntMap (a, a) -> IS.Key -> a
useRule 'L' m l = fst (m IM.! l)
useRule 'R' m l = snd (m IM.! l)
useRule _ _ _ = error "useRule: bad rule"

-- encode a 3 char string (all upper case characters) as an int
encode :: String -> Int
encode = foldl' (\acc c -> acc * 26 + ord c - ord 'A') 0

process :: String -> (String, IntMap (Int, Int))
process i = (instrs, rules)
  where
    [instrs, rules'] = splitOn "\n\n" i
    rules = IM.fromList $ map f $ lines rules'
    f (i1 : i2 : i3 : _ : _ : _ : _ : l1 : l2 : l3 : _ : _ : r1 : r2 : r3 : _) =
      (encode [i1, i2, i3], (encode [l1, l2, l3], encode [r1, r2, r3]))
    f _ = error "process: bad rule"

endsWith :: Int -> Int -> Bool
endsWith c k = k `mod` 26 == c

namesEndingIn :: Int -> IntMap (Int, Int) -> [Int]
namesEndingIn c m = filter (endsWith c) $ IM.keys m

cycleLength :: IntMap (Int, Int) -> String -> Int -> Int
cycleLength m rules pos = go (IS.fromList [pos]) 0 (cycle rules)
  where
    endCond = all ((== 25) . (`mod` 26)) . IS.toList
    go ns !n _ | endCond ns = n
    go ns !n (c : cs) = go (IS.map (useRule c m) ns) (n + 1) cs
    go _ _ [] = error "cycleLength: not possible"

part1 :: (String, IntMap (Int, Int)) -> Int
part1 (rules, m) = cycleLength m rules 0

part2 :: ([Char], IntMap (Int, Int)) -> Int
part2 (rules, m) = foldl1' lcm cycleLengths
  where
    allAs = namesEndingIn 0 m
    cycleLengths = map (cycleLength m rules) allAs

main :: IO ()
main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- process <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
