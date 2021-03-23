module FunctionEvaluator where

import qualified Data.Map as Map

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = error "todo: evaluateFunction"

evalMemo :: Ord a => Map.Map a b -> a -> b
evalMemo = undefined

-- test
fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)

