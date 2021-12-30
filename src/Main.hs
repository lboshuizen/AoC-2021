module Main where

import Day15.Chiton (chiton)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day15.txt"
  let r = chiton d
  --putStrLn r
  print r


