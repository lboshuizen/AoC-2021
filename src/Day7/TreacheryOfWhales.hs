module Day7.TreacheryOfWhales (treachery) where

import Data.List.Split (splitOn)
import Data.List (minimumBy, sortBy)

parse :: [String] -> [Int]
parse = map read . splitOn "," . head

linear, crab :: Int -> Int
linear n = n
crab n = n*(n+1) `div` 2

cost :: (Int->Int) -> [Int] -> Int -> Int
cost f nx n = sum $ f . abs . (n-) <$> nx

dist :: (Int->Int) -> [Int] -> [Int]
dist f xs = cost f xs <$> [0..(maximum xs)]

part1,part2 :: [Int] -> [Int]
part1 = dist linear
part2 = dist crab

treachery :: [String] -> Int
treachery = minimum . part2 . parse
