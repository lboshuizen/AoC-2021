module Main where

import Utils (readLines)
import           Day1.SonarSweep (sweep)

main :: IO ()
main = do
  r <- readLines "./data/day1.txt"
  let l = sweep r
  print l
