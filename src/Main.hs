module Main where

import Day25.SeaCucumber (sea)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day25.txt"
  let r = sea d
  --putStrLn r
  print r


