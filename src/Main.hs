module Main where

import           Day7.TreacheryOfWhales (treachery)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day7.txt"
  let r = treachery d
  print r

