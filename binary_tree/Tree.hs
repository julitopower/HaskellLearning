module Tree (newTree, Tree(Empty, Node), treeInsertList) where

{-
A Binary tree. It can either be Empty or a Node
-}
data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show

-- Build a new Empty binary Tree
newTree :: Tree a
newTree = Empty

-- Insert a value into a binary Tree
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Empty = Node Empty x Empty
treeInsert x node@(Node l v r)
  | x == v = node
  | x < v = Node (treeInsert x l) v r
  | otherwise = Node l v (treeInsert x r)

-- Create a binary tree out of the values on a list
treeInsertList :: Ord a => [a] -> Tree a
treeInsertList l = foldl (\t x-> treeInsert x t) (Empty) l
