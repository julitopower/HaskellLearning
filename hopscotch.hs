skips :: [a] -> [[a]]
skips [] = []
skips list = skipsAux [1..(length list)] list

skipsAux :: [Int] -> [a] -> [[a]]
skipsAux [] _ = []
skipsAux _ [] = []
{- 
1) Create list of tuples [(index, value)]
2) Filter tuples whose indexes are multiples a particular value
3) Extract the value out of the tuples via map
4) Keep doing this until the list of multiples is exausted
-}
skipsAux (x:xs) list = (map (\(a,b) -> b)
                            (filter (\(a,b) -> a `mod` x == 0) 
                                    (zip [1..(length list)] list))) 
                       : (skipsAux xs list) 
