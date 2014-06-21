localMaxima :: [Integer] -> [Integer]
localMaxima list
  | length list > 2 = map (\(_,b,_) -> b) (filter (\(a,b,c) -> a < b && b > c)(neighborList list))
  | otherwise = []

neighborList :: [Integer] -> [(Integer, Integer, Integer)]
neighborList (a:b:c:xs) = (a,b,c) : (neighborList (b:c:xs))
neighborList list = []