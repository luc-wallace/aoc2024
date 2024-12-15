module Day06 where

import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

data Guard = Guard {dir :: Direction, xpos :: Int, ypos :: Int} deriving (Show)

type Movement = State Guard

type PositionSet = Set (Int, Int)

data Direction = N | E | S | W deriving (Show)

type Bounds = (Int, Int)

rotate90 :: Direction -> Direction
rotate90 d = case d of
  N -> E
  E -> S
  S -> W
  W -> N

step :: PositionSet -> PositionSet -> Bounds -> Movement PositionSet
step obstacles visited (maxx, maxy) = do
  Guard d x y <- get
  let (nextx, nexty) = case d of
        N -> (x, y - 1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x - 1, y)
  if (nextx, nexty) `S.member` obstacles
    then modify $ \_ -> Guard (rotate90 d) x y
    else modify $ \_ -> Guard d nextx nexty
  newx <- gets xpos
  newy <- gets ypos
  if newx > maxx || newy > maxy || newx < 0 || newy < 0
    then pure visited
    else step obstacles (S.insert (newx, newy) visited) (maxx, maxy)

solve :: String -> (Int, Int)
solve s = (partOne, partTwo)
  where
    obstacles = S.fromList $ findAll '#' s
    guard = uncurry (Guard N) $ head $ findAll '^' s
    partOne = length $ evalState (step obstacles (S.fromList [(xpos guard, ypos guard)]) (129, 129)) guard
    partTwo = 0

findAll :: Char -> String -> [(Int, Int)]
findAll c s = concatMap findC $ zip (lines s) [0 ..]
  where
    findC (l, y) = mapMaybe (\(c', x) -> if c' == c then Just (x, y) else Nothing) $ zip l [0 ..]
