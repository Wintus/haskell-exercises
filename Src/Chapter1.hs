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
    points n = undefined {- Rewrite HERE! -}

    -- (3)
    mancircle :: Int -> [(Int, Int)]
    mancircle n = undefined {- Rewrite HERE! -}
