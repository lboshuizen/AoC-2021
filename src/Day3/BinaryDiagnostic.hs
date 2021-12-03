module Day3.BinaryDiagnostic (diagnostic) where

import Data.List (transpose)
import Data.Char (digitToInt)

complement :: [Int] -> [Int]
complement = map (\n -> 1-n)

bin2dec :: [Int] -> Int
bin2dec = foldl (\a c -> 2*a+c) 0

most :: Ord a => (a,a) -> Int
most (a,b) | a > b = 0
           | otherwise = 1

count :: [Int] -> (Int,Int)
count = foldr cnt (0,0)
        where cnt c (zr,on) = case c of
                                0 -> (zr+1,on)
                                _ -> (zr,on+1)

part1 :: [[Int]] -> Int
part1 = uncurry (*) . gammaeps . map (most . count) . transpose
    where gammaeps xs = (bin2dec xs, bin2dec . complement $ xs )

reduce :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
reduce eq = go eq 0
    where go _ _ (x:[]) = x
          go eq col xs = go eq (col+1) . filter (match crit) $ xs
            where
                bit = head . drop col
                crit = bit . map(count) . transpose $ xs
                match zo = eq (most zo) . bit

part2 :: [[Int]] -> Int
part2 xs = ox * co2
        where 
        ox  = bin2dec . reduce (==) $ xs
        co2 = bin2dec . reduce (/=) $ xs

--diagnostic :: [String] -> Int
diagnostic = part2 . map (map digitToInt)