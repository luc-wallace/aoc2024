module Main where

import Day01 (solve)
import Day02 (solve)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "no day provided"
    (d : _) -> do
      let day = read d :: Int
      input <- readFile $ printf "inputs/%02d.txt" day
      let solution = solutions !! (day - 1)
      let (partOne, partTwo) = solution input

      putStrLn $ printf "aoc 2024 day %d" day
      putStrLn $ printf "part 1: %d" partOne
      putStrLn $ printf "part 2: %d" partTwo

solutions :: [String -> (Int, Int)]
solutions =
  [ Day01.solve
  , Day02.solve
  ]
