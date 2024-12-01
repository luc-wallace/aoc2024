module Day01 where

import Data.List (sort)

solve :: String -> (Int, Int)
solve input = (partOne, partTwo)
  where
    lists@(l1, l2) = genLists input
    partOne = sum $ map (\(a, b) -> abs (a - b)) $ uncurry zip lists
    partTwo = sum $ zipWith getSimilarity l1 (replicate (length l1) l2)

genLists :: String -> ([Int], [Int])
genLists input =
  let pairs = map getPair $ lines input in (sort $ map fst pairs, sort $ map snd pairs)
  where
    getPair :: String -> (Int, Int)
    getPair s = let [n1, n2] = words s in (read n1, read n2)

getSimilarity :: Int -> [Int] -> Int
getSimilarity n l = n * length (filter (n ==) l)
