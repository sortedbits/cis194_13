
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
histogram :: [Integer] -> String
histogram xs = plotLine counts (maximum counts)
  where counts = (countAll xs)

plotLine :: [Integer] -> Integer -> String
plotLine counts 0 = "==========\n0123456789\n"
plotLine counts n = (doPlotLine counts n) ++ "\n" ++ (plotLine counts (n - 1))

doPlotLine :: [Integer] -> Integer -> String
doPlotLine [] _       = ""
doPlotLine (c : cs) n = (plotChar c n) ++ (doPlotLine cs n)

plotChar :: Integer -> Integer -> String
plotChar x n = if x >= n then "*"  else " "

countAll :: [Integer] -> [Integer]
countAll [] = replicate 10 0
countAll xs = map (countSingle xs) [0..9]

countSingle :: [Integer] -> Integer -> Integer
countSingle [] _ = 0
countSingle (x : xs) n
  | n == x    = 1 + countSingle xs n
  | otherwise = countSingle xs n
