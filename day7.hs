{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T


-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList


-- Start working down here
part1 i = undefined
part2 i = undefined


-- In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.

-- Every hand is exactly one type. From strongest to weakest, they are:

-- Five of a kind, where all five cards have the same label: AAAAA
-- Four of a kind, where four cards have the same label and one card has a different label: AA8AA
-- Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
-- Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
-- Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
-- One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
-- High card, where all cards' labels are distinct: 23456
process l = ws
  where
    ws = (\[hand,y] -> ((hand),read y :: Int)) (words l)
-- 32T3K 765
-- T55J5 684
-- KK677 28
-- KTJJT 220
-- QQQJA 483
-- This example shows five hands; each hand is followed by its bid amount. Each hand wins an amount equal to its bid multiplied by its rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the strongest hand. Because there are five hands in this example, the strongest hand will have rank 5 and its bid will be multiplied by 5.

-- So, the first step is to put the hands in order of strength:

-- 32T3K is the only one pair and the other hands are all a stronger type, so it gets rank 1.
-- KK677 and KTJJT are both two pair. Their first cards both have the same label, but the second card of KK677 is stronger (K vs T), so KTJJT gets rank 2 and KK677 gets rank 3.
-- T55J5 and QQQJA are both three of a kind. QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show,Eq,Ord)

handType :: [Char] -> HandType
handType l = case (freqs l) of
  m | (M.size m) == 1 -> FiveOfAKind
  m | (M.size m) == 2 -> if (elem 4 (M.elems m)) then FourOfAKind else FullHouse
  m | (M.size m) == 3 -> if (elem 3 (M.elems m)) then ThreeOfAKind else TwoPair
  m | (M.size m) == 4 -> OnePair
  m | (M.size m) == 5 -> HighCard

-- function to compare two hands, if they are the same type, compare the hands, otherwise compare the types
compareHand :: [Char] -> [Char] -> Ordering
compareHand h1 h2
  | handType h1 == handType h2 = compareCards h1 h2
  | otherwise = compare (handType h1) (handType h2)

-- In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.


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

compareHands :: [Char] -> [Char] -> Ordering
compareHands h1 h2
  | handType h1 == handType h2 = compareCards h1 h2
  | otherwise = compare (handType h1) (handType h2)

compareCards :: [Char] -> [Char] -> Ordering
compareCards h1 h2 = compare (map cardValue h1) (map cardValue h2)

compareCards2 :: [Char] -> [Char] -> Ordering
compareCards2 h1 h2 = compare (map cardValue2 h1) (map cardValue2 h2)

-- for part 2, J can be whatever kind of card that would make the hand the strongest
-- so we want to pairwise compare all the possible hands with J replaced with each of the other cards
-- expand the Js but remember they can be distinct, so JJ can be KA
-- expandJs :: [Char] -> [[Char]]
expandJs [] = [[]]
expandJs (c:cs)
  | c == 'J' = [(c':) cs | c' <- ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A' ], cs <- expandJs cs]
  | otherwise
    = [(c:)cs | cs <- expandJs cs]


-- However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be: JKKK2 is weaker than QQQQ2 because J is weaker than Q.

data WasJoker = WasJoker String | NotJoker deriving (Show,Eq)

maximizeHand :: [Char] -> ([Char], WasJoker)
maximizeHand h | elem 'J' h = (maximumBy compareHands2 (expandJs h), WasJoker h)
               | otherwise = (h,NotJoker)

    -- use compareHands2
    
    
compareHands2 :: [Char] -> [Char] -> Ordering
compareHands2 h1 h2
  | handType h1 == handType h2 = compareCards2 h1 h2
  | otherwise = compare (handType h1) (handType h2)
  
comparer ((h, j)) ((h2, j2))
  -- if same hand type, compare the hands
  | h == h2 = case (j, j2) of
      (NotJoker, NotJoker) -> compareCards2 h h2
      (NotJoker, WasJoker s) -> compareCards2 h s
      (WasJoker s, NotJoker) -> compareCards2 s h2
      (WasJoker s, WasJoker s2) -> compareCards2 s s2
  -- otherwise compare the hand types
  | otherwise = compare h h2

typeForHand h = handType (maximumBy compareHands2 (expandJs h))

main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map process . lines <$> readFile dayFilename
  -- let foo = inp :: [(HandType,Int)]
  let aa = (sortBy (compareHands `on` fst) inp)
  -- print aa
  -- Now, you can determine the total winnings of this set of hands by adding up the result of multiplying each hand's bid with its rank (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total winnings in this example are 6440.
  let totalWinnings  = sum $ zipWith (*) ((map snd aa)) [1..]
  print totalWinnings
        
  -- let bb = (sortBy (comparer `on` fst) (map (\(h,b) -> (maximizeHand h,b)) inp))
  let bb = (sortBy (compare `on` fst) (map (\(h,b) -> ((typeForHand h, map cardValue2 h),b)) inp))
  -- print bb
  let totalWinnings2  = sum $ zipWith (*) ((map snd bb)) [1..]
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
