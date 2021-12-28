module Main where

import           Day11.DumboOctopus (octopus)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day11.txt"
  let r = octopus d
  --let p = foldl (\a c -> a ++ show c ++ "\n") "" r
  --putStrLn p
  print r

