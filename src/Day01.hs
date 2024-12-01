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
  let pairs = map getPair $ lines input
      first = sort $ map fst pairs
      second = sort $ map snd pairs
   in (first, second)
  where
    getPair :: String -> (Int, Int)
    getPair s =
      let parts = words s
          n1 = read $ head parts
          n2 = read $ last parts
       in (n1, n2)

getSimilarity :: Int -> [Int] -> Int
getSimilarity n l = n * length (filter (n ==) l)
