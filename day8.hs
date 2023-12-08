{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Control.Applicative hiding ((<|>))
import Control.Monad
import Criterion.Main
import Control.Arrow
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import qualified Data.Graph as G
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as P

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ByteString splitOn
splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

-- Useful functions
findFirst :: Foldable f => (a -> Bool) -> f a -> a
findFirst f = fromJust . find f

slidingWindows :: Int -> [Int] -> [[Int]]
slidingWindows n l = take n <$> tails l

-- | Compute the average of the elements of a container.
avg :: (Integral c, Foldable t) => t c -> c
avg = uncurry div . foldl' (\(s, l) x -> (x + s, succ l)) (0, 0)

swap (a, b) = (b, a)

dup a = (a, a)

-- | Count the number of items in a container where the predicate is true.
countIf :: Foldable f => (a -> Bool) -> f a -> Int
countIf p = length . filter p . toList

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | map a function over elems satisfying a predicate
mapIf p f = foldl' (\xs x -> if p x then f x : xs else x : xs) []

-- | Set the element at index @n@ to @x@
setAt n x = (\(l, r) -> l ++ x : tail r) . splitAt n

-- | Like @findIndices@ but also return the element found for each index
findIndicesElem :: Foldable t => (a -> Bool) -> t a -> [(a, Int)]
findIndicesElem p = fst . foldl' go ([], 0 :: Int)
  where
    go (l, n) x
      | p x = ((x, n) : l, n + 1)
      | otherwise = (l, n + 1)

-- | Perturb a list's elements satisfying a predicate with a function
pertubationsBy :: (a -> Bool) -> (a -> a) -> [a] -> [[a]]
pertubationsBy p f l = [setAt n (f x) l | (x, n) <- findIndicesElem p l]

-- | Unconditional pertubation
pertubations :: (a -> a) -> [a] -> [[a]]
pertubations = pertubationsBy (const True)

-- | Generate all the segments of a list, O(n^2)
segs :: [a] -> [[a]]
segs = concatMap tails . inits

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

enum :: (Eq t, Num t) => t -> t -> t -> [t]
enum l s c
  | l == c = [l]
  | otherwise = l : enum (l + s) s c

range :: (Num a, Ord a) => a -> a -> [a]
range a b = enum a (signum (b - a)) b

-- Start working down here
tok :: P.ReadP b -> P.ReadP b
tok = (P.skipSpaces *>)


useRule 'L' m l = fst (m IM.! l)
useRule 'R' m l = snd (m IM.! l)

-- encode a 3 char string (all upper case characters) as an int
encode :: String -> Int
encode = foldl' (\acc c -> acc * 26 + ord c - ord 'A') 0

decode :: Int -> String
decode 0 = ""
decode n = decode (n `div` 26) <> [chr (n `mod` 26 + ord 'A')]

-- encode = id

-- process = id
process :: String -> ([Char], IntMap (Int, Int))
process i = (instrs, rules)
  where
    [instrs, rules'] = splitOn "\n\n" i
    rules = IM.fromList $ map (f ) $ lines rules'
    -- f :: String
    f (i1:i2:i3:_:_:_:_:l1:l2:l3:_:_:r1:r2:r3:_) = (encode [i1,i2,i3],(encode [l1,l2,l3],encode [r1,r2,r3]))
runRules2 :: IntMap (Int, Int) -> String -> Int
runRules2 m rules = go (IS.fromList (namesEndingIn 0 m)) 0 augRules
  where
    -- endCond :: [String] -> Bool
    endCond = all ((== 25) . (`mod` 26)) . IS.toList
    augRules :: String
    augRules = cycle rules
    go ns n _ | endCond ns = n
    go ns n (c:cs) = go (IS.map (useRule c m) ns) (n+1) cs

namesEndingIn c m = filter (\k ->  k `mod` 26 == c) $ IM.keys m

cycleLength m rules pos = go (IS.fromList $ [pos]) 0 augRules
  where
    -- endCond :: [String] -> Bool
    endCond = all ((== 25) . (`mod` 26)) . IS.toList
    augRules :: String
    augRules = cycle rules
    go ns n _ | endCond ns = n
    go ns n (c:cs) = go (IS.map (useRule c m) ns) (n+1) cs

main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- process <$> readFile dayFilename
  let m = snd inp
  let allAs = namesEndingIn 0 m
  let cycleLengths = (map (cycleLength m (fst inp)) allAs)
  print (cycleLength m (fst inp) 0)
  print (foldr lcm (minimum cycleLengths) cycleLengths)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
