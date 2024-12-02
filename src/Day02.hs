module Day02 where

solve :: String -> (Int, Int)
solve input = (partOne, partTwo)
  where
    partOne = length $ filter isSafe reports
    partTwo = length $ filter isSafeDamp reports
    reports :: [[Int]]
    reports = map (map read . words) $ lines input

isSafe :: [Int] -> Bool
isSafe r = validDiff && (increasing || decreasing)
  where
    diffs = zipWith (-) r (tail r)
    increasing = all (> 0) diffs
    decreasing = all (< 0) diffs
    validDiff = all ((\a -> a > 0 && a < 4) . abs) diffs

isSafeDamp :: [Int] -> Bool
isSafeDamp r = isSafe r || any isSafe (removals r)

removals :: [a] -> [[a]]
removals [] = []
removals (x : xs) = xs : map (x:) (removals xs)
