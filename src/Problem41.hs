module Problem41 (solution) where

import Data.List (permutations)
import Util.Numbers (Digits (..))
import Util.Primes (isPrime)

largestPandigital :: Int -> Maybe Int
largestPandigital k =
  let permNumbers = map glueDigits (permutations $ reverse [1 .. k])
   in case filter isPrime permNumbers of
        [] -> Nothing
        a -> Just (maximum a)

maybeConcat :: [Maybe a] -> [a]
maybeConcat [] = []
maybeConcat (Nothing : xs) = maybeConcat xs
maybeConcat ((Just x) : xs) = x : maybeConcat xs

solution :: IO Int
solution = do
  pure $ maximum (maybeConcat (map largestPandigital [1 .. 9]))
