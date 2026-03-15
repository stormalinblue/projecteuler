module Problem35 (solution, circles) where

import Data.List (inits, tails)
import qualified Data.Set as Set
import Util.Numbers (digits, glueDigits)
import Util.Primes (primes)

circles :: Int -> [Int]
circles x = tail (map glueDigits $ zipWith (++) (tails dig) (inits dig))
  where
    dig = digits x

isPrimeWith :: Set.Set Int -> Int -> Bool
isPrimeWith ps x = x `Set.member` ps

isCircularPrimeWith :: Set.Set Int -> Int -> Bool
isCircularPrimeWith ps x = all (isPrimeWith ps) (circles x)

pureSolution :: Int
pureSolution =
  let relevantPrimes = takeWhile (<= 1000000) primes
      relevantPrimesSet = Set.fromAscList relevantPrimes
      circularPrimes = filter (isCircularPrimeWith relevantPrimesSet) relevantPrimes
   in length circularPrimes

solution :: IO Int
solution = do
  pure pureSolution
