type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 orig dest temp = [(orig, dest)]
hanoi n orig dest temp = hanoi (n-1) orig temp dest ++ [(orig, dest)] ++ hanoi (n-1) temp dest orig

