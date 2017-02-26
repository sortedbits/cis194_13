{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- e1
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..] 

-- e2
fibs2 :: [Integer] 
fibs2 =  1 : (1 : (fib' 2 1 1))

fib' :: Integer -> Integer -> Integer -> [Integer]
fib' x x0 x1 = x2 : (fib' (x + 1) x1 x2) where
  x2 = x0 + x1
  
-- e3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList s = case s of
  Cons x s' -> x : streamToList s'

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))
 
-- e4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f sa = case sa of
  (Cons x sa') -> Cons (f x) (streamMap f sa')

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- e5
nats :: Stream Integer
nats = streamFromSeed (+1) 0


{-
1 interleaved with (2 interleaved with (3 interleaved with ( ... )))
see https://oeis.org/A001511
-}
ruler :: Stream Integer
ruler = go 0
  where go n = streamRepeat n `interleaveStreams` go (succ n)
  
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s1 s2 = case (s1, s2) of
  ((Cons x1 s1'), (Cons x2 s2')) -> Cons x1 (Cons x2 (interleaveStreams s1' s2'))


