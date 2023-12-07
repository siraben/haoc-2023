{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (toList), maximumBy)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

parseHand :: [Char] -> (String, Int)
parseHand l = ws
  where
    ws = (\[hand, y] -> (hand, read y)) (words l)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

handType :: String -> HandType
handType l = case freqs l of
  m | M.size m == 1 -> FiveOfAKind
  m | M.size m == 2 -> if 4 `elem` M.elems m then FourOfAKind else FullHouse
  m | M.size m == 3 -> if 3 `elem` M.elems m then ThreeOfAKind else TwoPair
  m | M.size m == 4 -> OnePair
  m | M.size m == 5 -> HighCard
  _ -> error "invalid hand"

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

expandJs :: String -> [String]
expandJs [] = [[]]
expandJs (c : cs)
  | c == 'J' = [c' : cs' | c' <- "23456789TQKA", cs' <- expandJs cs]
  | otherwise =
      [c : cs' | cs' <- expandJs cs]

compareHands2 :: String -> String -> Ordering
compareHands2 h1 h2
  | handType h1 == handType h2 = compareCards2 h1 h2
  | otherwise = compare (handType h1) (handType h2)

typeForHand :: String -> HandType
typeForHand h
  | 'J' `elem` h = handType (maximumBy compareHands2 (expandJs h))
  | otherwise = handType h

calcWinnings :: [(a, Int)] -> Int
calcWinnings l = sum $ zipWith (*) (map snd l) [1 ..]

part1 :: [(String, Int)] -> Int
part1 inp = calcWinnings (sortBy (compareHands `on` fst) inp)

part2 :: [(String, Int)] -> Int
part2 inp = calcWinnings (sortBy (compare `on` fst) (map (\(h, b) -> ((typeForHand h, map cardValue2 h), b)) inp))

main :: IO ()
main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map parseHand . lines <$> readFile dayFilename
  print $ part1 inp
  print $ part2 inp
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
