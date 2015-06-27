module Src.Chapter4
    (
      tri_number
    , tetration
    , index
    , even_odd
    , insert
    , isort
    , part_num
    ) where

    -- (1.1)
    tri_number :: Int -> Int
    tri_number 0 = 0
    tri_number n = n + tri_number (n - 1)

    -- (1.2)
    tetration :: Integer -> Integer -> Integer
    tetration _ 0 = 1
    tetration x n = x ^ tetration x (n - 1)

    -- (1.3)
    index :: Int -> [a] -> a
    index 0 (x:_)  = x
    index n (_:xs) = index (n - 1) xs

    -- (1.4)
    even_odd :: [Int] -> ([Int], [Int])
    even_odd [] = undefined {- Rewrite HERE! -}

    -- (2.1)
    insert :: Ord a => [a] -> a -> [a]
    insert [] y = undefined {- Rewrite HERE! -}

    -- (2.2)
    isort :: Ord a => [a] -> [a]
    isort [] = undefined {- Rewrite HERE! -}

    --- (3)
    part_num :: Int -> Int
    part_num n = undefined {- Rewrite HERE! -}
