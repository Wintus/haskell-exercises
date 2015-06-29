module Src.Chapter6
    (
      sortByFrequency
    , initialMap
    , infixPalindromicNumber
    , vernam
    ) where
    import Data.Ord
    import Data.List
    import qualified Data.Map as Map
    import Data.Char
    import Data.Bits
    import Control.Arrow ((&&&))

    -- (1.1)
    -- https://github.com/thoughtbot/til/blob/master/haskell/sorting-in-reverse-order.md
    sortByFrequency :: Ord a => [a] -> [a]
    sortByFrequency = map head . sortBy (comparing $ Down . length)
                        . group . sort

    -- (1.2)
    initialMap :: [String] -> Map.Map Char [String]
    initialMap = Map.fromListWith (++) . map (head &&& return)
                  . filter (not . null)

    -- (1.3)
    infixPalindromicNumber :: Int -> Int
    infixPalindromicNumber n = head [m | m <- [0..]
      , let m' = show m
      , reverse m' == m'
      , show n `isInfixOf` m']


    -- (2)
    vernam :: String -> String -> String
    vernam k s = undefined {- Rewrite HERE! -}
