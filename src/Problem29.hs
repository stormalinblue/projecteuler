module Problem29 (solution) where

import Data.List.Ordered (unionAll)

rawSolution :: [Integer]
rawSolution = unionAll [[a ^ b | b <- [(2 :: Integer)..100]] | a <- [(2 :: Integer) ..100]]

solution :: Int
solution = length rawSolution