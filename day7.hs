{-# OPTIONS_GHC -Wall #-}

import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M


-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList


-- Start working down here
part1 i = undefined
part2 i = undefined

parseHand l = ws
  where
    ws = (\[hand,y] -> (hand,read y :: Int)) (words l)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show,Eq,Ord)

handType :: String -> HandType
handType l = case freqs l of
  m | M.size m == 1 -> FiveOfAKind
  m | M.size m == 2 -> if 4 `elem` M.elems m then FourOfAKind else FullHouse
  m | M.size m == 3 -> if 3 `elem` M.elems m then ThreeOfAKind else TwoPair
  m | M.size m == 4 -> OnePair
  m | M.size m == 5 -> HighCard
  _ -> error "impossible"

-- function to compare two hands, if they are the same type, compare the hands, otherwise compare the types
compareHand :: String -> String -> Ordering
compareHand h1 h2
  | handType h1 == handType h2 = compareCards h1 h2
  | otherwise = compare (handType h1) (handType h2)

cardValue :: Char -> Int
cardValue c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 11
  | c == 'T' = 10
  | otherwise = digitToInt c

cardValue2 :: Char -> Int
cardValue2 c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 1
  | c == 'T' = 10
  | otherwise = digitToInt c

compareHands :: String -> String -> Ordering
compareHands h1 h2
  | handType h1 == handType h2 = compareCards h1 h2
  | otherwise = compare (handType h1) (handType h2)

compareCards :: String -> String -> Ordering
compareCards h1 h2 = compare (map cardValue h1) (map cardValue h2)

compareCards2 :: String -> String -> Ordering
compareCards2 h1 h2 = compare (map cardValue2 h1) (map cardValue2 h2)

expandJs [] = [[]]
expandJs (c:cs)
  | c == 'J' = [c' : cs | c' <- ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A' ], cs <- expandJs cs]
  | otherwise
    = [c : cs | cs <- expandJs cs]

compareHands2 :: String -> String -> Ordering
compareHands2 h1 h2
  | handType h1 == handType h2 = compareCards2 h1 h2
  | otherwise = compare (handType h1) (handType h2)

typeForHand h = handType (maximumBy compareHands2 (expandJs h))

main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map parseHand . lines <$> readFile dayFilename
  let aa = sortBy (compareHands `on` fst) inp
  let totalWinnings  = sum $ zipWith (*) (map snd aa) [1..]
  print totalWinnings
  let bb = sortBy (compare `on` fst) (map (\(h,b) -> ((typeForHand h, map cardValue2 h),b)) inp)
  let totalWinnings2  = sum $ zipWith (*) (map snd bb) [1..]
  print totalWinnings2
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
