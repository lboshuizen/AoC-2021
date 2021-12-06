module Day5.HydrothermalVenture (venture) where

import Data.List.Split (splitWhen,splitOn)
import Data.List (sort, group)
import Data.Maybe (isJust)

type Point = (Int,Int)
type Line = (Point,Point)

parse :: String -> Line
parse = pack . map ( pack . map stoi . splitOn ",") . splitOn "->"
    where pack (x:y:_) = (x,y)
          stoi s = read s :: Int

isHV :: Line -> Bool
isHV ((x,y),(x',y')) = x == x' || y == y'

comp' :: Ord a => a -> a -> Int
comp' a b | b > a = 1
          | b < a = -1 
          | otherwise = 0
 
points :: Line -> [Point]
points ((x1,y1),(x2,y2)) = [ (x1+n*dx,y1+n*dy) | n <- [0..(max (abs (x2-x1)) (abs (y2-y1) ))] ]
    where dx = comp' x1 x2
          dy = comp' y1 y2

count :: [[Point]] -> Int
count = length . filter (\xs -> length xs > 1) . group . sort . concat

part2 :: [Line] -> Int
part2 = count . map points

part1 :: [Line] -> Int
part1 = part2 . filter isHV

venture :: [String] -> Int
venture = part2 . map parse
