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

removals :: [Int] -> [[Int]]
removals arr = map removeN ns
  where
    removeN n = map fst . filter ((/= n) . snd) $ zip arr ns
    ns = [0 .. length arr - 1]
