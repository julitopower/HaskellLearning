{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1) + fib (n - 2))

fibs1 :: [Integer]
fibs1 = map (fib) [0..]

fibs2 :: [Integer]
fibs2 = 0 : zipWith (+) fibs2 (1:fibs2)

-- Streams
data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
  show s = show (take 100 (streamToList s))

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x b) = Cons (f x) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) (Cons a2 s2) = (Cons a1 (Cons a2 (interleaveStreams s1 s2)))

-- Infinite list of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Ruler function

ruler :: Stream Integer
ruler = interleaveStreams(streamRepeat 0) (streamMap (\x -> max2PowerDivide((x+1) *2)) nats)

max2PowerDivide :: Integer -> Integer
max2PowerDivide nb = max2PowerDivideAcum 0 nb

max2PowerDivideAcum :: Integer -> Integer -> Integer
max2PowerDivideAcum acc nb 
  | odd nb = acc
  | otherwise = max2PowerDivideAcum (acc +1) (nb `div` 2)
                
-- Generator functions for fibonaccy                

instance Num (Stream Integer) where
  fromInteger x = Cons x $ streamRepeat 0
  negate s = streamMap (\x -> -x) s
  (+) (Cons v1 s1) (Cons v2 s2) = Cons (v1 + v2) (s1 + s2)
  (*) (Cons v1 s1) b@(Cons v2 s2) = Cons (v1 * v2) ((streamMap (*v1) s2) + s1*b)  
  
instance Fractional (Stream Integer) where 
  (/) a@(Cons v1 s1) b@(Cons v2 s2) = Cons (v1 `div` v2) (streamMap (*(1 `div` v2)) (s1 - ((a/b) * s2)))
  
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

fibo :: Stream Integer
fibo = x / (1 -x -x^2)