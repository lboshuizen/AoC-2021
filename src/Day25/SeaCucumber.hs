module Day25.SeaCucumber (sea) where

import qualified Data.Map as M
import Data.List (sort)

t = [
    "v...>>.vv>",
    ".vv>>.vv..",
    ">>.>v>...v",
    ">>v>>.>.v.",
    "v>v.vv.v..",
    ">.>>..v...",
    ".vv..>.>v.",
    "v.v..>>v.v",
    "....v..v.>"
    ]

type XY = (Int,Int)
type Env = M.Map XY Char

indexXY :: Integral n => [[a]] -> [((n, n), a)]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

draw :: Integral a => ((a,a) -> String) -> [(a,a)] -> String
draw f xs = unlines [ concat [ f (x,y) | x <- ([0..mx]) ] | y <- ([0..my]) ]
    where (mx,my) = maxXY xs

maxXY :: Integral a => [(a,a)] -> (a,a)
maxXY = foldr (\(x',y') (x,y) -> (max x x', max y y')) (0,0)

build :: [String] -> Env
build = M.fromList . filter (\(_,c) -> c /= '.') . indexXY

moveEast, moveSouth :: Int -> Env -> Env
moveEast mx w = M.union s . foldr (move) M.empty $ M.assocs e
    where (e,s) = M.partition ((==) '>') w
          pn n = (n+1) `mod` (mx+1)
          move (p@(x,y),c) m' | M.member (pn x,y) w = M.insert p c m' -- blocked
                              | otherwise = M.insert (pn x,y) c m'  -- move

moveSouth mx w = M.union e . foldr (move) M.empty $ M.assocs s
    where (e,s) = M.partition ((==) '>') w
          pn n = (n+1) `mod` (mx+1)
          move (p@(x,y),c) m' | M.member (x,pn y) w = M.insert p c m' -- blocked
                              | otherwise = M.insert (x,pn y) c m'  -- move

step :: (Int,Int) -> Env -> Env
step (mx,my) = moveSouth my . moveEast mx

doWhile :: Eq a => ((a,a) -> Bool) -> (a -> a) -> a -> [(a,a)]
doWhile p f = takeWhile p . pair . iterate f
    where pair xs = zip xs $ drop 1 xs

paint env = draw cm . M.keys $ env
    where cm p = case M.lookup p env of
                 Just c -> [c]
                 Nothing -> "."

part1 :: Env -> Int     
part1 env = (+) 1 . length $ doWhile notEqual (step lim) env
    where lim = maxXY $ M.keys env
          notEqual = uncurry (/=)

sea = part1 . build
