module Day24.ArithmeticLogicUnit (alu) where

import qualified Data.Map as M
import Data.List.Split
import Data.Char
import Data.List (unfoldr)
import Debug.Trace (trace)

data Value  = Reg String 
            | Lit Int 
            deriving (Show)

data Ins = Inp String
         | Opr String Value (Int->Int->Int)

type Memory = M.Map String Int

type Alu = (Memory,[Int])

num2digits :: Integral a => a -> [a]
num2digits = reverse . unfoldr digit
    where digit 0 = Nothing
          digit n = Just (n `mod` 10, n `div` 10)

parse :: [String] -> [Ins]
parse xs = decode . splitOn " " <$> xs

value :: String -> Value
value s | num = Lit $ (read s :: Int)
        | otherwise = Reg s
        where num = (isDigit $ s !! 0) || '-' == (s !! 0)

decode :: [String] -> Ins
decode ("inp":r:_) = Inp r
decode ("add":r:v:_) = Opr r (value v) (+)
decode ("mul":r:v:_) = Opr r (value v) (*)
decode ("div":r:v:_) = Opr r (value v) (div)
decode ("mod":r:v:_) = Opr r (value v) (mod)
decode ("eql":r:v:_) = Opr r (value v) (eql)
    where eql a b | a==b = 1
                  | otherwise = 0

store :: Memory -> String -> Int -> Memory
store m r v = M.adjust (\_ -> v) r m

load :: Memory -> String -> Int
load m r = trace( "l: " ++ r) m M.! r

update :: Memory -> (Int->Int->Int) -> String -> Value -> Memory
update m f r (Reg r') = let v' = load m r'
                            v = load m r
                        in store m r $ f v v'
update m f r (Lit i) =  let v = load m r
                        in store m r $ f v i

step :: Ins -> Alu -> Alu
step (Inp r)     (m,[]) = (store m r 1, []) --edge case
step (Inp r)     (m,v:io) = (store m r v, io)
step (Opr r v f) (m,io)   = (update m f r v, io)

execute :: [Ins] -> Memory
execute =  fst . foldr step (m,io) . reverse
    where m = M.fromList [("w",0),("x",0),("y",0),("z",0)]
          io = num2digits 91398299697996 -- random try

getz = flip load "z"

alu = M.assocs . execute . parse