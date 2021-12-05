module Day4.Bingo (bingo) where

import Data.List (find, (\\))
import Data.Maybe (mapMaybe)
import Data.List.Split (splitWhen,splitOn)
import Debug.Trace (trace)
-- creates an XY map/chart
-- [".#.","#.."] => [
--                    ((0,0),'.'),((1,0),'#'),((2,0),'.'),
--                    ((0,1),'#'),((1,1),'.'),((2,1),'.')
--                  ]
-- Note: origin/xy:(0,0) is top left

type Card = [((Int,Int), Int)]

indexXY :: [[a]] -> [((Int,Int),a)] --indexXY :: [[a]] -> [((x,y),a)]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

parse :: [String] -> ([Int],[Card])
parse xs = (pool, cards)
    where 
          stoi s = read s :: Int
          pool = map (stoi) . splitOn "," . head $ xs
          cards = map (card) . splitWhen (=="") . drop 2 $ xs
          card = indexXY . map (map (stoi) . words)

numbers :: Card -> [Int]
numbers = map (snd)

row :: Int -> Card -> [Int]
row r = numbers . filter (\((_,y),_) -> y == r)

col :: Int -> Card -> [Int]
col r = numbers . filter (\((x,_),_) -> x == r)

full :: [Int] -> [Int] -> Bool
full d = all (\n -> elem n d)

sq :: Int -> Card -> Maybe ((Int,Int),Card)
sq n c = case find (\((_,_),v) -> v == n) c of
         Nothing -> Nothing
         Just (xy,_) -> Just (xy, c)

withSquare :: Int -> [Card] -> [((Int,Int), Card)]
withSquare n = mapMaybe (sq n)

fullRowOrCol :: [Int] -> ((Int,Int), Card) -> Bool
fullRowOrCol draw ((x,y),crd) = (fullcol x) || (fullrow y)
    where fullcol c = full draw . col c $ crd
          fullrow r = full draw . row r $ crd

score :: [Int] -> Card -> Int
score draw@(l:_) = (*) l . sum . filter (\n -> notElem n draw) . numbers

part1 :: ([Int],[Card]) -> Int
part1 (ns,crds) = go ns []
                  where go (n:xs) prev = case winners of
                                         [] -> go xs (n:prev)
                                         _  -> score (n:prev) . snd . head $ winners
                                         where winners = filter (fullRowOrCol (n:prev)) . withSquare n $ crds

part2 (ns,crds) = go ns [] crds
    where
        go (n:_) prev [c] = score (n:prev) $ c
        go (n:xs) prev cs = case winners of
                            [] -> go xs (n:prev) cs
                            _  -> go xs (n:prev) (cs \\ (snd <$> winners))
                            where winners = filter (fullRowOrCol (n:prev)) . withSquare n $ cs


bingo :: [String] -> Int
bingo = part2 . parse