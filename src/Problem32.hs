module Problem32 (solution) where

import Data.List (permutations)
import qualified Data.Set as Set
import Util.Numbers (Digits (glueDigits))

digitProducts :: [Int] -> [(Int, Int, Int)]
digitProducts xs =
  let pairs = [(glueDigits first, glueDigits second) | (first, second) <- map (`splitAt` xs) [1 .. (length xs - 1)]]
   in [(a, b, a * b) | (a, b) <- pairs]

correctDigitProducts :: [Int] -> [(Int, [(Int, Int)])]
correctDigitProducts xs =
  let pairs = [(glueDigits first, digitProducts second) | (first, second) <- map (`splitAt` xs) [1 .. (length xs - 2)]]
   in filter (not . null . snd) [(a, [(x, y) | (x, y, z) <- b, a == z]) | (a, b) <- pairs]

allCorrectDigitProducts :: [(Int, [(Int, Int)])]
allCorrectDigitProducts = concatMap correctDigitProducts (permutations [1 .. 9])

solution :: IO Int
solution =
  do
    pure $ sum (Set.toList (Set.fromList (map fst allCorrectDigitProducts)))
