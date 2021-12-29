module Main where

import Day13.TransparentOrigami (origami)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day13.txt"
  let r = origami d
  putStrLn r
  --print r


