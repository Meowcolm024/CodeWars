module RailFenceCipher (encode, decode) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

encode :: [a] -> Int -> [a]
encode xs n = zipList $ map (flip splitRail n) $ chunksOf (2 * n -2) xs

decode :: [a] -> Int -> [a]
decode = splitCode

splitRail :: [a] -> Int -> [[a]]
splitRail [] _ = []
splitRail xs n =
  let (l, r) = splitAt n (map pure xs)
   in zipWith (++) l $ if null r then (repeat []) else ([] : reverse ([] : r))

zipList :: [[[a]]] -> [a]
zipList [] = []
zipList ([] : _) = []
zipList xs = let (p, r) = unzip $ map (splitAt 1) xs in concat (concat p) ++ zipList r

splitCode :: [a] -> Int -> [a]
splitCode [] _ = []
splitCode xs n =
  let l = length xs
      (x, r) = l `divMod` (2 * n -2)
   in if r == 0
        then
          let (f, r1) = splitAt x xs
              (r2, t) = splitAt (l -2 * x) r1
              md = chunksOf (2 * x) r2
           in concatMap mergePiece (getPiece f md t)
        else
          let (f, r1) = splitAt (x + 1) xs
              (r2, t) = splitAt (l -2 * x -1) r1
              md = chunksOf (2 * x) r2
           in concatMap mergePiece (getPiece (init f) md t) ++ [last f]

getPiece :: [a] -> [[a]] -> [a] -> [(a, [[a]], a)]
getPiece [] _ _ = []
getPiece (f : fs) md (t : ts) =
  let (m, ms) = unzip $ map (splitAt 2) md
   in (f, m, t) : getPiece fs ms ts

mergePiece :: (a, [[a]], a) -> [a]
mergePiece (x, ms, y) = [x] ++ th ++ [y] ++ reverse rs
  where
    (th, rs) = helper ms ([], [])

    helper [] acc = acc
    helper ([] : _) acc = acc
    helper ms (l, t) = helper rs (l ++ f, t ++ r)
      where
        (th, rs) = unzip $ map (splitAt 2) ms
        [f, r] = transpose th
