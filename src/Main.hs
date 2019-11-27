module Main where

import Day1

main :: IO ()
main = do
  driftData <- readFile "/home/nihliphobe/projects/haskell/aoc2018/data/day1.txt"
  case parseDrifts driftData of 
    Left err -> fail (show err)
    Right drifts -> print $ findFirstDup drifts 
