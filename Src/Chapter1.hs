module Src.Chapter1
    (
      manlen
    , points
    , mancircle
    ) where

    -- (1)
    manlen :: (Int, Int) -> (Int, Int) -> Int
    manlen (a, b) (c, d) = abs(a - c) + abs(b - d)

    -- (2)
    points :: Int -> [(Int, Int)]
    points n = [(x, y) | x <- range, y <- range]
      where
        range = [(-n)..n]

    -- (3)
    mancircle :: Int -> [(Int, Int)]
    mancircle n = filter (\p -> manlen p (0, 0) == n) $ points n
