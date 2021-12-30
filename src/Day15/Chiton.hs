module Day15.Chiton (chiton) where

import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.List (foldl', sort)
import Data.Maybe (fromJust, catMaybes)

import qualified Data.Map.Strict as M
import Data.Char (digitToInt)   

type XY = (Int,Int)

github.com/tov/memoize

-- courtesy of Abhinav Sarkar
-- https://gist.github.com/abhin4v/8172534
astarSearch :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar (PQ.singleton (heuristic startNode) (startNode, 0))
         Set.empty (Map.singleton startNode 0) Map.empty
  where
    astar pq seen gscore tracks
      | PQ.null pq           = Nothing
      | isGoalNode node      = Just (gcost, findPath tracks node)
      | Set.member node seen = astar pq' seen gscore tracks
      | otherwise            = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq'           = PQ.deleteMin pq
        seen'         = Set.insert node seen
        successors    = 
          filter (\(s, g, _) -> not (Set.member s seen') &&
                    (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore)))
          $ successorsAndCosts node gcost
        pq''    = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts node gcost = map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node          = if Map.member node tracks
      then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
      else [node]

parse :: [String] -> [[Int]]
parse xs = (\s -> digitToInt <$> s) <$> xs

indexXY :: [[Int]] -> [(XY,Int)]
indexXY xs = concat $ [[((y, x), c) | (x, c) <- zip [0 ..] r] | (y, r) <- zip [0 ..] xs]

adjacents :: XY -> [XY]
adjacents (x,y) = (\(x',y') -> (x+x',y+y')) <$> [(-1,0),(1,0),(0,-1),(0,1)]

next :: M.Map XY Int -> XY -> [(XY,Int)]
next m = catMaybes . map l . adjacents
    where l p = case M.lookup p m of
                Nothing -> Nothing
                Just c -> Just (p,c)

path :: Int -> M.Map XY Int -> Maybe (Int, [XY])
path mx m = astarSearch (0,0) ((==) (mx,mx)) (next m) (\(x,_) -> 4*(mx-x))

expand :: Int -> Int -> [(XY,Int)] -> [(XY,Int)]
expand n m xs = concat [ [ ((x+m*ox,y+m*oy),level r ox oy) | ((x,y),r) <- xs, ox <- [0..n] ] | oy <- [0..n] ]
    where level r x y | s > 9 = s - 9
                      | otherwise = s
            where s = (r + x + y)

maxX xs = maximum $ fst . fst <$> xs

part1, part2 :: [(XY,Int)] -> Int
part1 xs = fst . fromJust . path (maxX xs) $ M.fromList xs

part2 xs = part1 . expand 4 mx $ xs
    where mx = 1 + maxX xs

chiton = part2 . indexXY . parse
