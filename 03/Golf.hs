
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
