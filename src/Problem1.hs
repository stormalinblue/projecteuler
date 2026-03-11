module Problem1 (solution) where

import qualified Data.List.Ordered as OrderedList

pureSolution :: Int
pureSolution = sum (takeWhile (< 1000) (OrderedList.union [3, 6 ..] [5, 10 ..]))

solution :: IO Int
solution = pure pureSolution
