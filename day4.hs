{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.Foldable
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- Start working down here
part1 :: [(IntSet, IntSet)] -> Int
part1 = sum . map (f . score)
  where
    f 0 = 0
    f n = 2 ^ (n - 1)

score :: (IntSet, IntSet) -> Int
score (a, b) = IS.size $ IS.intersection a b

part2 :: [(IntSet, IntSet)] -> Int
part2 inp = snd $ foldl' go (IM.fromList [(1, 1)], 0) l
  where
    l = zip [1 ..] inp
    go (cnt, tot) (n, g) = (IM.delete n cnt', tot + amt)
      where
        s = score g
        amt = IM.findWithDefault 1 n cnt
        toAdd
          | s == 0 = []
          | otherwise = [n + 1 .. (min (length l) (n + s))]
        cnt' = go2 toAdd cnt
          where
            -- no more cards to add
            go2 [] m = case IM.lookup n m of Nothing -> IM.insert n 1 m; _ -> m
            -- for each card to add, update its amount by number of instances of
            -- our current amount
            go2 (x : xs) m = go2 xs (IM.insertWith (\_ a -> a + amt) x (1 + amt) m)

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
