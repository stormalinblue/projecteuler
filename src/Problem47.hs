module Problem47 (solution) where

import Util.Primes (primeFactorization)

hasFourFactors :: Int -> Bool
hasFourFactors a = length (primeFactorization a) == 4

lastInSequence :: Int -> Int -> Int
lastInSequence current goingFor =
  let continuesStreak = hasFourFactors current
      result
        | goingFor == 4 && continuesStreak = current
        | continuesStreak = lastInSequence (current + 1) (goingFor + 1)
        | otherwise = lastInSequence (current + 1) 0
   in result

pureSolution :: Int
pureSolution = lastInSequence 0 0 - 3

solution :: IO Int
solution = do
  pure pureSolution
