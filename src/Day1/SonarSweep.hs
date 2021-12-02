module Day1.SonarSweep (sweep) where

increase :: Ord a => (a,a) -> Bool
increase (x,y) = y > x

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

window3sum :: [Int] -> [Int]
window3sum xs = zipWith3 (\a b c -> a+b+c) xs ys zs
    where ys = tail xs
          zs = tail ys

part1 :: [Int] -> Int
part1 = length . filter increase . pairs

part2 :: [Int] -> Int
part2 = part1 . window3sum

sweep :: [String] -> Int
sweep = part2 . map stoi
    where stoi s = read s :: Int