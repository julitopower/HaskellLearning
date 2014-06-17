-- Takes an integer are returns a list with the individual digits, reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n < 10 = [n]
  | otherwise =  (n `mod` 10) : (toDigitsRev (n `div` 10))

-- Take an integer and returns a list with the individual digits in the
-- order they were suplied
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [2*x]
--doubleEveryOther (x:y) = (2*x) : y
doubleEveryOther (x:(y:z)) = x : (2*y) : doubleEveryOther z

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum(toDigits x)
sumDigits (x:y) = sum(toDigits x) + sumDigits y

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10) == 0 