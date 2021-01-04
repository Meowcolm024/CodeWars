module TreeByLevels where

import Data.List (sortBy)

data TreeNode a = TreeNode
  { left :: Maybe (TreeNode a),
    right :: Maybe (TreeNode a),
    value :: a
  }
  deriving (Show)

data LvTree a = LvTree
  { lvLeft :: Maybe (LvTree a),
    lvRight :: Maybe (LvTree a),
    lvVal :: a,
    lvSize :: Int
  }
  deriving (Show)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = map fst . perf

restruct :: Maybe (TreeNode a) -> Int -> Maybe (LvTree a)
restruct Nothing _ = Nothing
restruct (Just (TreeNode l r v)) i = Just $ LvTree (restruct l (i + 1)) (restruct r (i + 1)) v i

toList :: Maybe (LvTree a) -> [(a, Int)]
toList Nothing = []
toList (Just (LvTree l r v i)) = (v, i) : toList l ++ toList r

perf :: Maybe (TreeNode a) -> [(a, Int)]
perf = sortBy (\(_, l) (_, r) -> l `compare` r) . toList . flip restruct 0
