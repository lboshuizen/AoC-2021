module Main where

import Day12.PassagePathing (pathing)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day12.txt"
  let r = pathing d
  print r


