module Day2.Dive (dive) where

parse :: String -> (String,Int)
parse s = (d, stoi i)
    where d:i:_ = words s
          stoi n = read n :: Int

move :: [(String,Int)] -> (Int,Int,Int)
move = foldl mv (0,0,0)
       where mv (x,y,a) cmd = case cmd of
                                ("forward",n) -> (x+n, y+(a*n), a)
                                ("up",n) ->      (x  , y      , a-n)
                                ("down",n) ->    (x  , y      , a+n)

dive :: [String] -> Int
dive = position . move . map (parse)
       where position (f,d,_) = f * d