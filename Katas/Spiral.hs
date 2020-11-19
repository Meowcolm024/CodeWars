module Spiral where

newtype ShowSpiral = ShowSpiral [[Int]] deriving Eq

instance Show ShowSpiral where
  show (ShowSpiral sp) = unlines $
    ["<samp>"] ++ 
    [[if x == 1 then '0' else '.' | x <- row] | row <- sp] ++
    ["</samp>"]

spiralize :: Int -> [[Int]]
spiralize n = unsnail n $ start n

start :: Int -> [Int]
start = tail . concat . flip gen True 

gen :: Num a => Int -> Bool -> [[a]]
gen x n | x < 2 = pure $ rep (x+1) n
gen x n = rep x n : rep (x-1) n : rep (x-1) n : rep (x-2) n : gen (x-2) (not n)

rep :: Num a => Int -> Bool -> [a]
rep x n = if n then replicate x 1 else replicate x 0

unsnail :: Int -> [Int] -> [[Int]]
unsnail _ [] = []
unsnail 2 [a,b,c,d] = case (c, d) of
  (0, 1) -> [[a,b], [0,0]]
  (1, 0) -> [[a,b], [0,1]]
  _ -> [[a,b], [d,c]]
unsnail l xs = 
  let (p, ya) = splitAt l xs
      (mr, yb) = splitAt (l-2) ya
      (q, yc) = splitAt l yb
      (ml, yd) = splitAt (l-2) yc
  in p : zipWith3 (\k ys r -> k : ys ++ [r]) (reverse ml) (unsnail (l-2) yd) mr ++ [q]
