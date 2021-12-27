module Main where

import           Day9.SmokeBasin (smoke)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day9.txt"
  let r = smoke d
  print r

