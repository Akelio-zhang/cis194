{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

-- ex1 --

toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- ex2 --

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = case lst of 
    [] -> []
    [x] -> [x]
    lst -> doubleEveryOther (init (init lst)) ++ [2*(last (init lst)),last lst]
--version2    (reverse -> (x:y:xs)) ->  doubleEveryOther (reverse xs) ++ [2*y, x]

-- ex3 --
sumDigits :: [Integer] -> Integer
sumDigits lst = case lst of
    [] -> 0
    (x:xs) -> sum (toDigits x) + sumDigits xs

-- ex4 --
validate :: Integer -> Bool
validate num = sumDigits (doubleEveryOther (toDigits num)) `mod` 10 == 0


-- ex5 --
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a,b)]
    | n >= 2 = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)
    | otherwise = []

