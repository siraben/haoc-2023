{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.Bifunctor
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

-- set of location of symbols
type SymbolSet = Set (Int, Int)

-- Number location is
-- (r, c, length, number)
type NumberLoc = (Int, Int, Int, Int)

-- a symbol is a character that is not a number or a dot
isSym :: Char -> Bool
isSym c = not (isDigit c) && c /= '.'

mkGrid :: [[Char]] -> [((Int, Int), Char)]
mkGrid ss = distr (zip [0 ..] (map (zip [0 ..]) ss))
  where
    distr :: [(Int, [(Int, Char)])] -> [((Int, Int), Char)]
    distr [] = []
    distr ((r, cs) : rs) = map g cs ++ distr rs
      where
        g (c, s) = ((r, c), s)

mkSymSet :: [((Int, Int), Char)] -> SymbolSet
mkSymSet = S.fromList . map fst . filter (isSym . snd)

mkStars :: [((Int, Int), Char)] -> SymbolSet
mkStars = S.fromList . map fst . filter ((== '*') . snd)

mkNum1 :: Int -> [Char] -> (Int, [(Int, Char)])
mkNum1 r s = (r, lbld)
  where
    lbld = mapMaybe f [(col, c) | (col, c) <- zip [0 ..] s]
      where
        f (col, c)
          | isDigit c = pure (col, c)
          | otherwise = Nothing

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' _ [x] = [[x]]
groupBy' cmp (x : xs@(x' : _))
  | cmp x x' = (x : y) : ys
  | otherwise = [x] : r
  where
    r@(y : ys) = groupBy' cmp xs

foobar :: [(Int, [[(Int, Char)]])] -> [[(Int, Int, Int, Int)]]
foobar = map (\(r, l) -> map ((\(b, c) -> (r, b, length c, read c :: Int)) . foldr (\(col, c) (_, s) -> (col, c : s)) (0, "")) l)

barbaz :: [(Int, [(Int, Char)])] -> [(Int, [[(Int, Char)]])]
barbaz = map (second (groupBy' (\(a, _) (b, _) -> b - a == 1)))

process :: [String] -> [NumberLoc]
process = concat . foobar . barbaz . zipWith mkNum1 [0 ..]

isPart :: (Int, Int, Int, Int) -> SymbolSet -> Bool
isPart (r, c, l, _) symbols =
  S.fromList
    ( map (r + 1,) [c - 1 .. c + l]
        ++ map (r - 1,) [c - 1 .. c + l]
        ++ [(r, c - 1), (r, c + l)]
    )
    `S.intersection` symbols
    /= S.empty

explode :: (Int, Int, Int, Int) -> M.Map (Int, Int) Int
explode (r, c, l, n) = M.fromList [((r, x), n) | x <- [c .. c + l - 1]]

-- 8 distance
starNeighbors :: (Int, Int) -> [(Int, Int)]
starNeighbors (r, c) =
  [ (r + 1, c),
    (r - 1, c),
    (r, c + 1),
    (r, c - 1),
    (r + 1, c + 1),
    (r + 1, c - 1),
    (r - 1, c + 1),
    (r - 1, c - 1)
  ]

getNumbs :: (Int, Int) -> M.Map (Int, Int) Int -> [Int]
getNumbs (r, c) m = nub $ mapMaybe (m M.!?) (starNeighbors (r, c))

part1 (pp, ss, grid) = sum $ map (\(_, _, _, d) -> d) (filter (`isPart` ss) pp)

part2 (pp, ss, grid) = sum $ map product $ filter ((== 2) . length) $ map (`getNumbs` foldl' (\m' p -> M.union (explode p) m') M.empty (filter (`isPart` ss) pp)) (S.toList stars)
  where
    stars = mkStars grid

main :: IO ()
main = do
  let dayNumber = 3 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let grid = mkGrid inp
  let ss = mkSymSet grid
  let pp = process inp
  let inp' = (pp, ss, grid)
  print $ part1 inp'
  print $ part2 inp'
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
