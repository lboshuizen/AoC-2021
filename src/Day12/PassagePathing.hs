module Day12.PassagePathing (pathing) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (isLower)
import Data.Maybe (catMaybes)

type Graph = M.Map String ([String],Int)

parse :: String -> (String,String)
parse = tpl . splitOn "-"
    where tpl (l:r:_) = (l,r)

bidir xs = xs ++ map (\(a,b) -> (b,a)) xs

addNode :: (String,String) -> Graph -> Graph
addNode (s,e) = M.alter f s
    where f Nothing = Just ([e],0)
          f (Just (xs,i)) = Just (e:xs,i)

buildGraph :: [String] -> Graph
buildGraph = foldr addNode M.empty . bidir . map parse

terminal "start" = True
terminal "end" = True
terminal _ = False

isSmall = isLower . head

rm :: Ord k => ((k,a) -> Bool) -> M.Map k a -> M.Map k a
rm p m = foldr M.delete m . map fst . filter p . M.assocs $ m

visit :: String -> Graph -> Graph
visit k m | terminal k = M.delete k m

visit k m | isSmall k = case m M.! k of
                        (p,2) -> M.adjust (\(p,_) -> (p,1)) k m
                        (p,1) -> M.map (\(p,_) -> (p,0)) . rm claimed $ m
                        (p,0) -> M.delete k m
                        where claimed (_,(_,i)) = i == 1

visit _ m = m

exits :: Graph -> String -> [String]
exits m k = case M.lookup k m of
            Nothing -> []
            Just (p,_) -> p

move :: ([String],Graph) -> String -> Maybe ([String],Graph)
move (xs,m) t | M.member t m = Just (t:xs,visit (head xs) m)
              | otherwise = Nothing

path :: ([String],Graph) -> String -> [Maybe ([String],Graph)]
path (xs,m) "end" = [move (xs,m) "end"]
path (xs,m) e = let ns = move (xs,m) e
                in case ns of
                    Nothing -> [Nothing]
                    Just ns'@(_,m') -> concat $ path ns' <$> exits m' e

part1, part2 :: String -> Graph -> Int
part1 s m = length . catMaybes . concat $ path ([s],m) <$> exits m s

part2 s = part1 s . M.map (\(xs,_) -> (xs,2))

pathing = part2 "start" . buildGraph