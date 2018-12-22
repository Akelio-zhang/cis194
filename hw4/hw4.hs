{-# OPTIONS_GHC -Wall #-}

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


