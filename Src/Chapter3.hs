module Src.Chapter3
    (
      tri_pattern
    , tri_guard
    , tri_case
    , qadd
    , qequal
    , qlist
    ) where

    -- (1.1)
    tri_pattern :: Int -> Int
    tri_pattern 0 = 0
    tri_pattern 1 = 0
    tri_pattern 2 = 1
    tri_pattern n = tri_pattern (n - 1) + tri_pattern (n - 2)
                      + tri_pattern (n - 3)

    -- (1.2)
    tri_guard :: Int -> Int
    tri_guard n
      | n < 2     = 0
      | n == 2    = 1
      | otherwise =
          tri_pattern (n - 1) + tri_pattern (n - 2) + tri_pattern (n - 3)

    -- (1.3)
    tri_case :: Int -> Int
    tri_case n = case n of
      0 -> 0
      1 -> 0
      2 -> 1
      _ -> tri_pattern (n - 1) + tri_pattern (n - 2) + tri_pattern (n - 3)

    -- (2.1)
    qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
    qadd q1 q2 = undefined {- Rewrite HERE! -}

    -- (2.2)
    qequal :: (Int, Int) -> (Int, Int) -> Bool
    qequal q1 q2 = undefined {- Rewrite HERE! -}

    -- (2.3)
    qlist :: (Int, Int) -> [(Int, Int)]
    qlist q = undefined {- Rewrite HERE! -}
