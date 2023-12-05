module Day04 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split

getNumbers :: String -> [Int]
getNumbers s = map read (words s)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input-test.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines $ map (unlines . splitOneOf "|:") inputLines
  putStrLn "TODO: implement Day 04"