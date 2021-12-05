module Day4.Bingo (bingo) where

import Data.List.Split (splitWhen,splitOn)
import Debug.Trace (trace)
-- creates an XY map/chart
-- [".#.","#.."] => [
--                    ((0,0),'.'),((1,0),'#'),((2,0),'.'),
--                    ((0,1),'#'),((1,1),'.'),((2,1),'.')
--                  ]
-- Note: origin/xy:(0,0) is top left
indexXY :: Integral n => [[a]] -> [((n,n),a)]
--indexXY :: [[a]] -> [((x,y),a)]
indexXY xs = concat $ [[((x, y), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

row :: Integral n => [((n,n),a)] -> Int -> [a]
row board r = [] 

col :: Integral n => [((n,n),a)] -> Int -> [a]
col board c = [] 

marked :: Eq a => [a] -> a -> Bool 
marked xs n = n `elem` xs

--winner :: Eq a => [a] -> [((n,n),a)] -> Bool
--winner xs = all (\n -> n elem xs)  . map snd

--cross :: Eq a => [a] -> [((n,n),a)] -> Bool
--cross xs = any (\n -> n elem xs)  . map snd

--parse :: Integral n => [String] -> ([Int],[((n,n),a)])
--parse :: [String] -> ([Int],[[String]])

dropEmpty = splitWhen (=="")

stoi s = read s :: Int

--parse :: [String] -> ([Int],[[String]])
parse xs = (pool, boards)
    where 
          pool = map (stoi) . splitOn "," . head $ xs
          boards = map (board) . splitWhen (=="") . drop 2 $ xs
          board = indexXY . map (map (stoi) . words)
--scan :: Integral n => [[((n,n),a)]] -> n -> Maybe [a]

--bingo :: [String] -> ([Int],[String])
bingo  = parse