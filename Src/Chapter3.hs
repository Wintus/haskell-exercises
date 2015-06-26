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
    qadd (_, 0) (_, _) = error "Zero Division"
    qadd (_, _) (_, 0) = error "Zero Divison"
    qadd (a, b) (c, d) = (a * d + b * c, b * d)

    -- (2.2)
    qequal :: (Int, Int) -> (Int, Int) -> Bool
    qequal (_, 0) (_, _) = error "Zero Divsion"
    qequal (_, _) (_, 0) = error "Zero Divsion"
    qequal (a, b) (c, d) = a * d == b * c

    -- (2.3)
    qlist :: (Int, Int) -> [(Int, Int)]
    qlist (_, 0)         = error "Zero Division"
    qlist (a, b) | b < 0 = qlist (-a, -b)
    qlist (a, b)         =
      let g  = gcd a b
          a' = a `div` g
          b' = b `div` g
      in  [(s * a' * n, s * b' * n) | n <- [1..], s <- [1, -1]]
