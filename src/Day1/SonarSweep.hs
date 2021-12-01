module Day1.SonarSweep (sweep) where

import Utils (stoi)

dist :: (Int,Int) -> Int
dist (x,y) = y-x

sum3 :: (Int,Int,Int) -> Int
sum3 (x,y,z) = x+y+z

positive :: Int -> Bool
positive n = n > 0

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

triples :: [a] -> [(a,a,a)]
triples xs = zip3 xs ys zs
    where ys = tail xs
          zs = tail ys

part1 :: [Int] -> Int
part1 = length . filter positive . map (dist) . pairs

part2 :: [Int] -> Int
part2 = part1 . map (sum3) . triples

sweep :: [String] -> Int
sweep = part2 . map stoi