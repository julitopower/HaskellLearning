data Tree = 
     Leaf 
     | Node Tree Int Tree
     deriving Show

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x tree@(Node left value right)
       | x > value = Node left value (insert x right)
       | x < value = Node (insert x left) value right
       | otherwise = tree  

insertList :: [Int] -> Tree -> Tree
insertList [] tree = tree
insertList (h:t) tree = insert h (insertList t tree) 

createTree :: [Int] -> Tree
createTree x = insertList (reverse x) Leaf

contains :: Int -> Tree -> Bool
contains x Leaf = False
contains x tree@(Node left value right)
	 | x > value = contains x right
	 | x < value = contains x left
	 | otherwise = True