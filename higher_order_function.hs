fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x - 2) * y ) 1 . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
                     
fun2' :: Integer -> Integer               
fun2' = sum . filter (even) . takeWhile (> 1) . iterate (\x -> if (even x) then (x `div` 2) else (3 * x + 1))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)
                 
foldTree :: [a] -> Tree a                 
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 0 Leaf x Leaf
addNode x (Node h left value right)
  | hl < hr = Node h (addNode x left) value right
  | hl > hr = Node h left value newRight 
  | otherwise = Node (hnr + 1) left value newRight
 where hl = height left
       hr = height right
       newRight = addNode x right
       hnr = height newRight
    
height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h