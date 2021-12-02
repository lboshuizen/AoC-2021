module Main where

import           Day3.BinaryDiagnostic (diagnostic)
import           Utils                 (readLines)

t = [
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010"
      ]

main :: IO ()
main = do
  r <- readLines "./data/day3.txt"
  let l = diagnostic r
  print l
