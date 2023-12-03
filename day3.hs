{-# OPTIONS_GHC -Wall #-}

import Data.Char
import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bifunctor
import Data.List


-- set of location of symbols
type SymbolSet = Set (Int, Int)

-- check if range of numbers (x1,y1), (x2,y2) is valid
-- adjacent to a symbol


-- file looks like this
-- 467..114..
-- ...*......
-- ..35..633.
-- ......#...
-- 617*......
-- .....+.58.
-- ..592.....
-- ......755.
-- ...$.*....
-- .664.598..

-- Number location is
-- (r, c, length, number)
type NumberLoc = (Int, Int, Int, Int)

-- a symbol is a character that is not a number or a dot
isSym :: Char -> Bool
isSym c = not (isDigit c) && c /= '.'

mkGrid ss = distr (zip [0..] (map (zip [0..]) ss))
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

-- mkNum1 :: Int -> [Char] -> [(Int, Int, Char)]
mkNum1 r s = (r, lbld)
  where
    lbld = mapMaybe f [(col, c) | (col, c) <- zip [0..] s]
      where
        f (col, c) | isDigit c = pure (col, c)
                   | otherwise = Nothing

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _   []                        = []
groupBy' _   [x]                       = [[x]]
groupBy' cmp (x:xs@(x':_)) | cmp x x'  = (x:y):ys
                           | otherwise = [x]:r
  where r@(y:ys) = groupBy' cmp xs

-- mkNum :: [(Int, Int, Char)] -> [NumLoc]
-- mkNum l =
-- map (groupBy' (\(_,a,_) (_,b,_) -> b - a == 1)

foobar :: [(Int, [[(Int, Char)]])] -> [[(Int, Int, Int, Int)]]
foobar = map (\(r,l) -> map ((\(b,c) -> (r,b,length c, read c :: Int)) . foldr (\(col,c) (_, s) -> (col,c:s)) (0,"")) l)

barbaz = map (second (groupBy' (\ (a, _) (b, _) -> b - a == 1)))

process :: [[Char]] -> [NumberLoc]
process = concat . foobar . barbaz . zipWith mkNum1 [0..]

-- numIsSym :: NumberLoc -> SymbolSet -> Bool
-- numIsSym (r, c, l, n) symbols = checkRange c r (c + l - 1) r
--   where
--     checkRange = undefined

-- check that max(c-1,0) to c+l-1 
isPart (r,c,l,n) symbols = S.fromList
  (map (r + 1,) [c-1..c+l] ++
   map (r - 1,) [c-1..c+l] ++
   [(r,c-1),(r,c+l)]) `S.intersection` symbols /= S.empty

explode (r,c,l,n) = M.fromList [((r,x), n) | x <- [c..c+l-1]]

-- 8 distance
starNeighbors :: (Int, Int) -> [(Int, Int)]
starNeighbors (r,c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1),
                       (r + 1, c + 1), (r + 1, c - 1), (r - 1, c + 1), (r - 1, c - 1)]

getNumbs (r,c) m = nub $ mapMaybe (\p -> m M.!? p) (starNeighbors (r,c))

main = do
  let dayNumber = 3 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  -- print (take 10 inp)
  let grid = mkGrid inp
  -- print grid
  let ss = mkSymSet grid
  let stars = mkStars grid
  print ss
  -- print (process inp)
  let ns = filter (`isPart` ss) (process inp)
  print ns
  let nss = foldl' (\m' p -> M.union (explode p) m') M.empty ns
  print $ sum $ map product $ filter ((== 2) . length) $ map (\p -> getNumbs p nss) (S.toList stars)
  -- print $ map (\p -> getNumbs p (explode ns)) (S.toList stars)
  print $ sum $ map (\(_,_,_,d) -> d) (filter (`isPart` ss) (process inp))

  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
