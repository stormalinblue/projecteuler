module Problem36 (solution) where

import Util.Numbers (Digits (..))

is10Palindrome :: Int -> Bool
is10Palindrome x = revDigits x == digits x

is2Palindrome :: Int -> Bool
is2Palindrome x = revBinDigits x == binDigits x

pureSolution :: Int
pureSolution = sum $ filter (\x -> is10Palindrome x && is2Palindrome x) [1, 3 .. 1000000 - 1]

solution :: IO Int
solution = do pure pureSolution
