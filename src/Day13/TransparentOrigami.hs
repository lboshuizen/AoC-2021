module Day13.TransparentOrigami (origami) where

import Data.List.Split (splitOn, splitWhen)
import Data.List (nub, sort)

parse :: [String] -> ([(Int,Int)],[(String,Int)])
parse xs = (dots,flds)
    where (dts:ins:_) = splitWhen (=="") xs
          dots = (\(x:y:_) -> (stoi x, stoi y)) . splitOn "," <$> dts
          flds = (\(x:y:_) -> (x, stoi y)) . splitOn "=" . drop 11 <$> ins
          stoi s = read s :: Int

draw :: [(Int,Int)] -> String
draw xs = unlines [ concat [if (x,y) `elem` xs then "X" else " " | x <- ([0..mx]) ] | y <- ([0..my]) ]
    where (mx,my) = last . sort $ xs

swap :: (a,a) -> (a,a)
swap (x,y) = (y,x)

fx :: Int -> (Int,Int) -> (Int,Int)
fx n (x,y) | x > n = (n-(x-n),y)
fx _ xy = xy

foldx, foldy :: Int -> [(Int,Int)] -> [(Int,Int)]
foldx n xs = fx n <$> xs
foldy n xs = swap . fx n . swap <$> xs

fl :: (String,Int) -> [(Int,Int)] -> [(Int,Int)]
fl ("x",n) = nub . foldx n
fl ("y",n) = nub . foldy n

part1 :: ([(Int,Int)],[(String,Int)]) -> Int
part1 (xn,xi) = length . foldr fl xn $ take 1 xi

part2 :: ([(Int,Int)],[(String,Int)]) -> String
part2 (xn,xi) = draw . foldr fl xn $ reverse xi

origami = part2 . parse