{-# OPTIONS_GHC -Wall #-}

-- e1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
  . filter even
  . takeWhile (/=1)
  . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- e2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = foldr add Leaf xs
 
add :: a -> Tree a -> Tree a
add x Leaf                  = Node 0 Leaf x Leaf
add x (Node 0 Leaf v Leaf)  = Node 1 (add x Leaf) v Leaf
add x (Node _ Leaf v right) = Node 1 (add x Leaf) v right
add x (Node _ left v Leaf)  = Node 1 left v (add x Leaf)
add x (Node h left@(Node hl _ _ _) v right@(Node hr _ _ _))
  | hl < hr   = Node h (add x left) v right
  | hl > hr   = Node h left v (add x right)
  | otherwise = Node (h' + 1) left v right'
      where
        right' = (add x right)
        (Node h' _ _ _ ) = right'
            
      
