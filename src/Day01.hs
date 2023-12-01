{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char

getDigits :: String -> String
getDigits s = [c | c <- s, isDigit c]

twoDigits :: String -> Int
twoDigits s | length s >2    = read $ head s:[last s]
            | length s == 1  = read $ replicate 2 (head s)
            | otherwise      = read s

-- sumDigits :: String -> Int
-- sumDigits s | length s >2       =  head (map digitToInt s) + last (map digitToInt s)
--             | length s == 2     = sum (take 2 (map digitToInt s))
--             | otherwise         = 2 * digitToInt (head s)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ unlines (map getDigits inputLines)
  let s = sum $ map (twoDigits . getDigits) inputLines
  print s
