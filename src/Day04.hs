module Day04 where

import Data.List (isPrefixOf, transpose)

solve :: String -> (Int, Int)
solve s = (partOne, partTwo)
  where
    l = lines s
    partOne = sum $ map countXmasWord (l ++ allDiags l ++ allDiags (map reverse l) ++ (reverse . transpose) l)
    partTwo = sum $ map countXmasShape $ divide3x3 l

diag :: [[a]] -> [a]
diag x = zipWith (!!) x [0 ..]

allDiags :: [[a]] -> [[a]]
allDiags [] = []
allDiags xxs = init $ shrink xxs tail init ++ shrink xxs init tail
  where
    shrink [] _ _ = []
    shrink x f1 f2 = shrink (f2 $ map f1 x) f1 f2 ++ [diag x]

countXmasWord :: String -> Int
countXmasWord [] = 0
countXmasWord s = countXmasWord (tail s) + if "XMAS" `isPrefixOf` s || "SAMX" `isPrefixOf` s then 1 else 0

divide3x3 :: [[a]] -> [[[a]]]
divide3x3 xs
  | length xs < 3 || length (head xs) < 3 = []
  | otherwise =
      [ [take 3 (drop j row) | row <- take 3 (drop i xs)]
        | i <- [0 .. (length xs - 3)],
          j <- [0 .. (length (head xs) - 3)]
      ]

countXmasShape :: [String] -> Int
countXmasShape x = if (mas . diag) x && (mas . diag) ((reverse . transpose) x) then 1 else 0
  where
    mas s = "MAS" `isPrefixOf` s || "SAM" `isPrefixOf` s
