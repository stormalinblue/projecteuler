module Problem46 (goldbachDecomposition, rawSolution, solution) where

import Data.Maybe (isNothing)
import Util.Numbers (isPrime)

goldbachDecomposition :: Int -> Maybe (Int, Int)
goldbachDecomposition a =
  let decompositions = [(a - 2 * x * x, x) | x <- takeWhile (\k -> 2 * k * k <= a) [0 ..], isPrime (a - 2 * x * x)]
   in if null decompositions then Nothing else Just (head decompositions)

rawSolution :: (Int, Maybe (Int, Int))
rawSolution = head (filter (isNothing . snd) (map (\x -> (x, goldbachDecomposition x)) [3, 5 ..]))

solution :: Int
solution = fst rawSolution
