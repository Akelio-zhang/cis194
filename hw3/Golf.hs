{-# OPTIONS_GHC -Wall #-}

-- ex1 --
mapHelper :: [a] -> Int -> [a]
mapHelper l n
    | length l + 1 <= n = []
    | otherwise = case l of
                [] -> []
                _ -> l!!(n-1) : mapHelper (drop n l) n
skips :: [a] -> [[a]]
skips l = map (mapHelper l) [1..length l]

-- ex2 --
localMaxima :: [Integer] -> [Integer]
localMaxima l = case l of
    t@(x:y:z:_) -> if y > x && y > z then y : localMaxima (tail t) else localMaxima (tail t)
    _ -> []


-- ex3 --
count :: [Integer] -> [Int]
count l = map (\n -> length (filter (\x -> x == n) l)) [0..9]
toCntStr :: [Int] -> Int -> String
toCntStr l n = (map (\x -> if x >= n then '*' else ' ') l)
histogram :: [Integer] -> String
histogram l = unlines (filter (\x -> '*' `elem` x) (map helper [9,8..1]))
                ++ "==========\n0123456789\n"
                where helper = toCntStr (count l)
