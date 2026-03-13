module Problem23 (solution, abundantNumbers, abundantNumbersLeq, nonAbundantSumsLeq, nonAbundantSums) where

import Data.List (inits)
import qualified Data.Set as Set
import Util.Primes (factorSum, factorSumWithPrimes, primes)

isAbundant :: (Integral a) => a -> Bool
isAbundant a = (a + a) < factorSum a

abundantNumbers :: (Integral a) => [a]
abundantNumbers = filter isAbundant [1 ..]

infCross :: [a] -> [a] -> [(a, a)]
infCross _ [] = []
infCross [] _ = []
infCross allX allY =
  concat (zipWith (\x y -> zip x (reverse y)) (inits allX) (inits allY))

abundantSummable :: (Integral a) => a -> Bool
abundantSummable x =
  any isAbundant [x - a | a <- takeWhile (< x) abundantNumbers]

nonAbundantSums :: (Integral a) => [a]
nonAbundantSums = filter (not . abundantSummable) [1 ..]

abundantNumbersLeq :: (Integral a) => a -> [a]
abundantNumbersLeq lim =
  let relevantPrimes = takeWhile (<= lim + 1) primes
      isAbundantWithPrimes a = (a + a) < factorSumWithPrimes relevantPrimes a
   in filter isAbundantWithPrimes [1 .. lim]

nonAbundantSumsLeq :: (Integral a) => a -> [a]
nonAbundantSumsLeq a =
  let abundants = abundantNumbersLeq a
      abundantsSet = Set.fromAscList abundants
      abundantSum x = any (`Set.member` abundantsSet) [x - b | b <- takeWhile (< x) abundants]
   in filter (not . abundantSum) [1 .. a]

solution :: IO Int
solution = do
  pure $ sum (nonAbundantSumsLeq 28123)
