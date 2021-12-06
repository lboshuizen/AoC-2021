module Main where

import           Day5.HydrothermalVenture (venture)

t = [
    "1,1 -> 1,3",
    "9,7 -> 7,7",
    "1,1 -> 3,3",
    "9,7 -> 7,9"
    ]

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  r <- readLines "./data/day5.txt"
  let l = venture r
  print l
