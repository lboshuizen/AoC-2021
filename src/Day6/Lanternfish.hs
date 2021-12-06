module Day6.Lanternfish (lanternfish) where

import Data.List (span, replicate, sort, group)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

parse :: String -> [(Int,Int)]
parse =  map (\xs -> (head $ xs, length $ xs)) . group . sort . map read . splitOn ","

gen :: [(Int,Int)] -> [(Int,Int)]
gen xs = ((6,new):(8,new):surv)
    where 
        (z,nz) = span (\(n,_) -> n==0) . sort $ xs
        surv = map (\(n,c) -> (n-1,c)) $ nz
        new = sum . map snd $ z

grow :: Int -> [(Int,Int)] -> [(Int,Int)]
grow 0 xs = xs
grow n xs = grow (n-1) . gen $ xs 

part1 :: [(Int,Int)] -> [(Int,Int)]
part1 = grow 80

part2 :: [(Int,Int)] -> [(Int,Int)]
part2 = grow 256

lanternfish = sum . map (snd) . part2 . parse . head