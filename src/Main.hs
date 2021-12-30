module Main where

import Day24.ArithmeticLogicUnit (alu)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day24.txt"
  let r = alu d
  --putStrLn r
  print r


