module Main where

import Day14.ExtendedPolymerization (polymerization)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day14.txt"
  let r = polymerization d
  --putStrLn r
  print r


