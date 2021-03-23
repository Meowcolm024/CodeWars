{-# LANGUAGE RankNTypes #-}
module LambdaCalculusLists (prepend,append) where

import Prelude hiding (head,tail)
-- import Preloaded (Boolean(..),false,true,Pair(..),pair,List(..),nil,isEmpty,head,tail)

prepend :: List x -> x -> List x
prepend xs x = List (pair false (pair x xs)) 

append :: List x -> x -> List x
append xs x = runBoolean (isEmpty xs) (prepend nil x) (prepend (append (tail xs) x) (head xs)) 

-- Preloaded.hs

newtype Boolean = Boolean { runBoolean :: forall a. a -> a -> a }

false,true :: Boolean
false = Boolean $ \ t f -> f
true  = Boolean $ \ t f -> t

newtype Pair x y = Pair { runPair :: forall z. (x -> y -> z) -> z }

pair :: x -> y -> Pair x y
pair x y = Pair $ \ z -> z x y

first :: Pair x y -> x
first (Pair xy) = xy $ \ x y -> x

second :: Pair x y -> y
second (Pair xy) = xy $ \ x y -> y

newtype List x = List { runList :: Pair Boolean (Pair x (List x)) }

nil :: List x
nil = List $ pair true undefined

isEmpty :: List x -> Boolean
isEmpty (List xs) = first xs

head :: List x -> x
head (List xs) = first $ second xs

tail :: List x -> List x
tail (List xs) = second $ second xs