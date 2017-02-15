
module Golf where

-- e1 hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (get xs) [1..(length xs)]
  
get :: [a] -> Int -> [a]
get [] _ = []
get xs n
  | n > length xs  = []
  | otherwise      = head (drop (n - 1) xs) : get (drop n xs) n  

-- e2 local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima []             = []
localMaxima (x1 : [])      = []
localMaxima (x1 : x2 : []) = []
localMaxima (x1 : x2 : x3 : xs)
  | x1 < x2 && x2 > x3 = x2 : localMaxima (x2 : x3 : xs)
  | otherwise          = localMaxima (x2 : x3 : xs)

-- e3 histogram
-- histogram :: [Integer] -> String
