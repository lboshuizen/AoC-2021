module Main where

import           Day4.Bingo (bingo)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  r <- readLines "./data/day4.txt"
  let l = bingo r
  print l
