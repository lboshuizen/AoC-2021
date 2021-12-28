module Day11.DumboOctopus (octopus) where

import qualified Data.Map as M
import Data.Char (digitToInt)   

type XY = (Int,Int)
type Cave = M.Map XY Int

parse :: [String] -> [[Int]]
parse xs = (\s -> digitToInt <$> s) <$> xs

indexXY :: [[Int]] -> [(XY,Int)]
indexXY xs = concat $ [[((y, x), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

prep :: [String] -> Cave
prep = M.fromList . indexXY . parse

tgts :: [XY] -> [XY]
tgts xs = concat $ adjacents <$> xs
    where adjacents (x,y) = [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], (x',y') /= (0,0) ]

flash :: Cave -> (Cave,Cave) -> Cave
flash d (fs,dm) | M.null fs = M.union (clr d) dm
                | otherwise = flash (M.union fs d) . M.partition (>9) . incHit . tgts . M.keys $ fs
                where incHit = foldr (\p a -> M.adjust ((+)1) p a) dm
                      clr = M.map (\_ -> 0)

step :: Cave -> Cave
step = flash M.empty . M.partition (>9) . M.map ((+)1)

flashed = length . filter (==0) . M.elems

part1, part2 :: Cave -> Int
part1 m = fst $ foldr go (0,m) [0..99]
    where go _ (c,m') = (c+c',next)
            where
                next = step m'
                c' = flashed next

part2 m = go 1 m
    where go n m' | all (==0) $ M.elems next = n
                  | otherwise = go (n+1) next
            where next = step m'

octopus = part2  . prep