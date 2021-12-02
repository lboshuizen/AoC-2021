module Day3.BinaryDiagnostic (diagnostic) where

import Data.List (transpose)
import Data.Char (digitToInt)

complement :: [Int] -> [Int]
complement = map (\n -> 1-n)

bin2dec :: [Int] -> Int
bin2dec = foldl (\a c -> 2*a+c) 0

common :: [Char] -> Int
common = max . foldr cnt (0,0)
         where cnt c (zr,on) = case c of
                             '0' -> (zr+1,on)
                             _ -> (zr,on+1)
               max (a,b) | a > b = 0
                         | otherwise = 1

select :: (Int -> Int -> Bool) -> Int -> [[Char]] -> [[Char]]
select eq n xs = filter match $ xs
            where dominant = common . position n . transpose $ xs
                  match xs' = eq (digitToInt . position n $ xs') dominant
                  position n = head . drop n

reduce :: (Int -> Int -> Bool) -> [[Char]] -> [Char]
reduce eq = go eq 0
            where
                go _ _ (x:[]) = x
                go eq n xs = go eq (n+1) . select eq n $ xs

criteria :: (Int -> Int -> Bool) -> [[Char]] -> Int
criteria eq = bin2dec . map (digitToInt) . reduce eq

part1 = uncurry (*) . gammaeps . map (common) . transpose
        where gammaeps xs = (bin2dec xs, bin2dec . complement $ xs)

part2 xs = uncurry (*) (oxygen xs, co2 xs)
        where oxygen = criteria (==)
              co2 = criteria (/=)

diagnostic :: [String] -> Int
diagnostic = part2