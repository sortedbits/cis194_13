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
