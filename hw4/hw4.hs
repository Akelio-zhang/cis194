{-# OPTIONS_GHC -Wall #-}
import Data.List
-- ex1 --
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . 
    iterate (\x -> if even x then x `div` 2 else 3*x+1 )

-- ex2 --
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

isDeeper :: Tree a -> Tree b -> Bool
isDeeper lt rt = case lt of
        Leaf -> False
        Node x _ _ _ -> case rt of
                    Leaf -> True
                    Node y _ _ _ -> if x >= y then True else False
addNode :: a -> Tree a -> Tree a
addNode node tree = case tree of
                Leaf -> Node 0 Leaf node Leaf
                Node _ lt val rt -> if isDeeper lt rt
                                      then let newTree = addNode node rt
                                                in case newTree of
                                                    Leaf -> error "impossible!"
                                                    Node dp _ _ _ -> Node (dp+1) lt val newTree
                                      else let newTree = addNode node lt
                                                in case newTree of
                                                    Leaf -> error "impossible!"
                                                    Node dp _ _ _ -> Node (dp+1) newTree val rt

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

-- ex3 --
xor :: [Bool] -> Bool
xor = foldl (\acc t -> if t == True then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- ex4 --
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ nums)
            where nums = map (\(i,j) -> i + j + 2*i*j) ( filter (\(i,j) -> i + j + 2*i*j <= n)
                        ( cartProd [1..n] [1..n] ))

