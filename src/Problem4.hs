module Problem4 (solution) where

import Util.Numbers (Digits (digits, revDigits))

isPalindromic :: Int -> Bool
isPalindromic x = revDigits x == digits x

threeDigitNumbers :: [(Int, Int)]
threeDigitNumbers = [(i, j) | i <- [100 .. 999], j <- [i .. 999]]

pureSolution :: Int
pureSolution = maximum (filter isPalindromic (map (uncurry (*)) threeDigitNumbers))

solution :: IO Int
solution = do
  pure pureSolution
