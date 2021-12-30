module Main where

import Day16.PacketDecoder (decoder)

readLines :: FilePath -> IO [String]
readLines fname =
  do
    content <- readFile fname
    let ls = lines content
    return ls

main :: IO ()
main = do
  d <- readLines "./data/day16.txt"
  let r = decoder d
  --putStrLn r
  print r


