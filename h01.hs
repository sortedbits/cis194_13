-- e1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (mod n 10) : toDigitsRev (div n 10)

-- e2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x : xs) 
  | (mod (length xs + 1) 2 == 0) = (2 * x) : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs

-- e3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10    = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

-- e4
validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0

-- e5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ (hanoi 1 a b c) ++ hanoi (n - 1) c b a

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- hanoi 3 "a" "b" "c" == [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
