module Day04 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split
import qualified Data.Set as S

getNumbers :: String -> [Int]
getNumbers s = map read (words s)

commonNums :: [[Int]] -> S.Set Int
commonNums ss = S.fromList (head ss) `S.intersection` S.fromList  (last ss)

score :: Int -> Int
score 0 = 0
score n = 2 ^ (n - 1)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines $ map (unlines . splitOneOf "|:") inputLines
  let cards = map (tail . splitOneOf "|:") inputLines
  let cardSets = map (map getNumbers) cards
  print $ sum $ map (score . S.size . commonNums) cardSets