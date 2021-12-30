module Day16.PacketDecoder (decoder) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (splitAt, foldl')

htb = [ ('0',[0,0,0,0]), ('1',[0,0,0,1]), ('2',[0,0,1,0]), ('3',[0,0,1,1]), ('4',[0,1,0,0]),
        ('5',[0,1,0,1]), ('6',[0,1,1,0]), ('7',[0,1,1,1]), ('8',[1,0,0,0]), ('9',[1,0,0,1]),
        ('A',[1,0,1,0]), ('B',[1,0,1,1]), ('C',[1,1,0,0]), ('D',[1,1,0,1]), ('E',[1,1,1,0]),
        ('F',[1,1,1,1]) ]

data Packet = Literal  { version :: Int, value :: Int }
            | Operator { version :: Int, tp :: Int, subs :: [Packet] } 
            -- deriving (Show)

b2i :: [Int] -> Int
b2i = foldl' (\n b -> 2*n+b) 0

parse :: String -> [Int]
parse = foldr (\c a -> (tr M.! c) ++ a) []
    where tr = M.fromList htb

litValue :: [Int] -> ([Int],[Int])
litValue (1:t) = let (b4,r) = splitAt 4 t
                     (n4,r') = litValue r
                 in  (b4 ++ n4, r')

litValue (_:t) = splitAt 4 t 

packets :: [Int] -> [Packet]
packets s = case packet s of
            Just (ap,r') -> ap : packets r'
            Nothing -> []

packet :: [Int] -> Maybe (Packet,[Int])
packet s | length s < 6 = Nothing
packet s = let typ = b2i tp in
           case typ of
           4 -> Just $ literal (b2i ver) typ tx
           _ -> Just $ operator (b2i ver) typ tx
        where (ver, vx) = splitAt 3 s
              (tp, tx)  = splitAt 3 vx 

literal, operator :: Int -> Int -> [Int] -> (Packet,[Int])
literal v _ s = (Literal v (b2i value),r)
    where (value,r) = litValue s

operator v t [] = (Operator v t [], [])

operator v tp (0:t) = (Operator v tp p, r')
    where (numBits,r) = (b2i $ take 15 t, drop 15 t)
          (pb,r') = splitAt numBits r
          p = packets pb

operator v tp (1:t) = (Operator v tp (reverse p), r')
    where (numPackets,r) = (b2i $ take 11 t, drop 11 t)
          (p,r') = foldr takePacket ([],r) [0..numPackets-1]
          takePacket _ (p,r) = case packet r of
                               Just (ap,r') -> (ap:p,r')

sumversions :: [Packet] -> Int
sumversions [] = 0 
sumversions ((Operator v _ p):t) = v + sumversions (p ++ t)
sumversions ((Literal v _) :t) = v + sumversions t 

eval :: Packet -> Int
eval (Literal _ v) = v
eval (Operator _ 0 xs) = sum $ eval <$> xs
eval (Operator _ 1 xs) = product $ eval <$> xs
eval (Operator _ 2 xs) = minimum $ eval <$> xs
eval (Operator _ 3 xs) = maximum $ eval <$> xs

eval (Operator _ 5 (l:r:_)) | eval(l) > eval(r) = 1
eval (Operator _ 6 (l:r:_)) | eval(l) < eval(r) = 1
eval (Operator _ 7 (l:r:_)) | eval(l) == eval(r) = 1

eval (Operator _ _ _) = 0

part1, part2 :: [Packet] -> Int
part1 = sumversions
part2 = eval . head

decoder :: [String] -> Int
decoder = part2 . packets . parse . head