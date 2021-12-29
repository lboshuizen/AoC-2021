module Day14.ExtendedPolymerization (polymerization) where

import qualified Data.Map as M
import Data.List.Split (splitOn)

type Rules = M.Map String Char
type PairCounter = M.Map String Int
type CharCounter = M.Map Char Int

parse :: [String] -> (String, Rules)
parse xs = (head xs, M.fromList rs)
    where rs = pair . splitOn " -> " <$> drop 2 xs
          pair (a:b:_) = (a,head b)

count :: Ord k => [k] -> M.Map k Int
count = counter M.empty

counter :: Ord k => M.Map k Int -> [k] -> M.Map k Int
counter m = foldr (M.alter cnt) m

add :: Int -> Maybe Int -> Maybe Int
add n (Just v) = Just (v+n)
add n _ = Just n

cnt :: Maybe Int -> Maybe Int
cnt = add 1

pairs :: String -> PairCounter
pairs xs = count . map ts . zip xs $ drop 1 xs
    where ts (a,b) = [a,b]

expand :: Rules -> a -> (PairCounter,CharCounter) -> (PairCounter,CharCounter)
expand r _ (cp,ec) = foldr go (M.empty,ec) $ M.assocs cp
    where go (s@(c1:c2:_),i) (np,lc) = case M.lookup s r of 
                                       Nothing -> (np,lc)
                                       Just c -> let nps = [ [c1,c], [c,c2] ]
                                                 in (updatePairs nps, updateChars)
                                                 where updatePairs xs = foldr (\s m -> M.alter (add i) s m ) np xs
                                                       updateChars = M.alter (add i) c lc

sol :: Int -> Rules -> String -> Int
sol n rules poly = diff . M.elems . snd . foldr (expand rules) (pc,cc) $ [0..(n-1)]
    where pc = pairs poly
          cc = count poly
          diff xs = (maximum xs) - (minimum xs)

part1, part2 :: (String, Rules) -> Int
part1 (poly,rules) = sol 10 rules poly
part2 (poly,rules) = sol 40 rules poly

polymerization = part2 . parse