{-# OPTIONS_GHC -Wall #-}

import Criterion.Main
import Data.Foldable
import Data.Functor
import qualified Text.ParserCombinators.ReadP as P

type Move = (Int, Int, Int)

part1 :: (Num a) => [(a, [Move])] -> a
part1 games = sum (map fst (filter (isGamePossible . snd) games))

part2 :: [(a, [Move])] -> Int
part2 games = sum (map (minCubeSet . snd) games)

data Color = R | G | B deriving (Show, Eq, Ord)

int :: P.ReadP Int
int = P.readS_to_P reads

tok :: P.ReadP b -> P.ReadP b
tok = (P.skipSpaces *>)

parseMove :: P.ReadP [(Color, Int)]
parseMove = do
  let c = do
        n <- int
        m <- tok (P.choice [P.string "red" $> R, P.string "green" $> G, P.string "blue" $> B])
        pure (m, n)
  P.sepBy c (P.char ',' *> P.skipSpaces)

parseGameLine :: P.ReadP [[(Color, Int)]]
parseGameLine = P.string "Game" *> int *> P.string ":" *> P.skipSpaces *> P.sepBy parseMove (P.char ';')

isMovePossible :: Move -> Bool
isMovePossible (r, g, b) = r <= 12 && g <= 13 && b <= 14

isGamePossible :: [Move] -> Bool
isGamePossible = all isMovePossible

minCubeSet :: [Move] -> Int
minCubeSet = (\(r, g, b) -> r * g * b) . foldl' (\(r, g, b) (r', g', b') -> (max r r', max g g', max b b')) (0, 0, 0)

mkMove :: [(Color, Int)] -> Move
mkMove =
  foldl'
    ( \(r, g, b) (c, n) -> case c of
        R -> (r + n, g, b)
        G -> (r, g + n, b)
        B -> (r, g, b + n)
    )
    (0, 0, 0)

main :: IO ()
main = do
  let dayNumber = 2 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let games' = zip ([1 ..] :: [Int]) (map (fst . last . P.readP_to_S parseGameLine) inp)
  let games = [(i, map mkMove ms) | (i, ms) <- games']
  print (part1 games)
  print (part2 games)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 games,
          bench "part2" $ whnf part2 games
        ]
    ]
