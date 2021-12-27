module Day9.SmokeBasin (smoke) where

import Data.List (find, nub, sort)
import Data.Set (Set, fromList, member, delete, empty, union, toList, notMember, (\\), size)
import qualified Data.Set as S (foldr)
import Data.Char (digitToInt)   
import Data.Maybe (catMaybes)

type XY = (Int,Int)
type Point = (XY,Int)

parse :: [String] -> [[Int]]
parse xs = line <$> xs
    where 
        line s = digitToInt <$> s

indexXY :: [[Int]] -> [Point]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

neighbours :: [Point] -> ((Int,Int),a) -> [Point]
neighbours xs (xy,_) = catMaybes . map (\p' -> find (\(xy',_) -> p'==xy') xs) . adjacents $ xy
    where
        adjacents (x,y) = map (\(x',y') -> (x+x',y+y')) [(-1,0),(1,0),(0,-1),(0,1)]

low :: [Point] -> Point -> Maybe Point
low xs p@(_,h) = let r = filter ((<=h) . snd) . neighbours xs $ p
                 in case r of
                    [] -> Just p
                    _ -> Nothing

lows :: [Point] -> [Point]
lows xs = catMaybes $ low xs <$> xs

neighboursM :: Set XY -> XY -> [XY]
neighboursM m xy = filter (\p -> member p m) . adjacents $ xy
    where
        adjacents (x,y) = map (\(x',y') -> (x+x',y+y')) [(-1,0),(1,0),(0,-1),(0,1)]

expand m d [] = d
expand m d xs = expand m (union d news) (toList news)
    where nbs = fromList . concat . map (neighboursM m)
          news = (nbs xs) \\ d

basins :: Set XY -> [Set XY] -> [Point] -> [Set XY]
basins m acc [] = acc
basins m acc (p:xs) = basins (m \\ l) (l:acc) xs
    where l = expand m empty [fst p]

part1, part2 :: [Point] -> Int
part1 xs = sum $ (+1) . snd <$> (lows xs)

part2 xs = result . basins noWalls [] . lows $ xs
    where
        result = product . take 3 . reverse . sort . map size
        noWalls = fromList . map fst . filter ((<9) . snd) $ xs

smoke = part2 . indexXY . parse