import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Graph as G
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as P

swap (a, b) = (b, a)

part1 games = sum (map fst (filter (isGamePossible . snd) games))

part2 games = sum (map (minCubeSet . snd) games)

data Color = R | G | B deriving (Show, Eq, Ord)

type Moves = [(Int, Color)]

type Game = [Moves]

int :: P.ReadP Int
int = P.readS_to_P reads

tok = (P.skipSpaces *>)

parseMove :: P.ReadP [(Color, Int)]
parseMove = do
  let c = (,) <$> int <*> tok (P.choice [P.string "red" $> R, P.string "green" $> G, P.string "blue" $> B])
  P.sepBy (swap <$> c) (P.char ',' *> P.skipSpaces)

parseGameLine = P.string "Game" *> int *> P.string ":" *> P.skipSpaces *> P.sepBy parseMove (P.char ';')

isMovePossible :: Map Color Int -> Bool
isMovePossible m = m M.!? R <= Just 12 && m M.!? G <= Just 13 && m M.!? B <= Just 14

isGamePossible = all isMovePossible

minCubeSet :: [Map Color Int] -> Int
minCubeSet = (\(r, g, b) -> r * g * b) . foldl' (\(r, g, b) m -> (max r (M.findWithDefault 0 R m), max g (M.findWithDefault 0 G m), max b (M.findWithDefault 0 B m))) (0, 0, 0)

main = do
  let dayNumber = 2 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let games = zip [1 ..] (map (map M.fromList . fst . last . P.readP_to_S parseGameLine) inp)
  print (part1 games)
  print (part2 games)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 games,
          bench "part2" $ whnf part2 games
        ]
    ]
