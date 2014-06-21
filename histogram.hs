h :: [Int] -> String
h list = ph (zip [0..9] (histInRange [0..9] list))
  
ph :: [(Int,Int)] -> String
ph [] = []
ph ((idx, value):xs) = (show idx) ++ "|" ++ (replicate value '*') ++ "\n" ++ ph xs
                


histInRange :: [Int] -> [Int] -> [Int]
histInRange [] _ = []
histInRange _ [] = []
histInRange (x:xs) list = (length (filter (== x) (list))) : (histInRange xs list) 