module Day05 where

import Data.List (partition, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)

type Rule = (Int, Int)

type RuleSet = Map Rule ()

type Update = [Int]

solve :: String -> (Int, Int)
solve s = (partOne, partTwo)
  where
    (rules, updates) = let (s1, s2) = span (/= "") $ lines s in (M.fromList $ map (\a -> (pRule a, ())) s1, map pUpdate $ tail s2)
    (valid, invalid) = partition (isValidUpdate rules) updates
    partOne = (sum . map mid) valid
    partTwo = (sum . map (mid . sortUpdate rules)) invalid

pRule :: String -> Rule
pRule s = let (x, y) = span (/= '|') s in (read x, read $ tail y)

pUpdate :: String -> Update
pUpdate s = map read $ splitOn "," s

mid :: [a] -> a
mid a = a !! ((length a - 1) `div` 2)

isValidUpdate :: RuleSet -> Update -> Bool
isValidUpdate r u = all (\(x, y) -> isNothing $ M.lookup (y, x) r) (zip u $ tail u)

sortUpdate :: RuleSet -> Update -> Update
sortUpdate r = sortBy (\x y -> if isJust $ M.lookup (y, x) r then GT else LT)
