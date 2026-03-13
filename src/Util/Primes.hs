module Util.Primes
  ( allPrimesIn,
    isPrime,
    primeFactorizationWithPrimes,
    primeFactorization,
    primes,
    factors,
    factorSum,
    factorSumWithPrimes,
  )
where

import Util.Numbers (divisible)

isPrime :: (Integral a) => a -> Bool
isPrime a
  | a <= 1 = False
  | otherwise = null [x | x <- takeWhile (\n -> n * n <= a) [2 ..], divisible a x]

primeFactorizationWithPrimes :: (Integral a) => [a] -> a -> [(a, Int)]
primeFactorizationWithPrimes allPrimes x =
  let pf n
        | n <= 1 = []
        | otherwise =
            let countDivides :: (Integral a) => a -> a -> Int -> (Int, a)
                countDivides b p accum
                  | b `divisible` p =
                      countDivides (b `div` p) p (1 + accum)
                  | otherwise = (accum, b)

                pfInner _ [] = []
                pfInner b (p : ps)
                  | b < p = []
                  | b `divisible` p =
                      let (count, remaining) = countDivides b p 0
                       in (p, count) : pfInner remaining ps
                  | otherwise = pfInner b ps
             in pfInner n allPrimes
   in pf x

primeFactorization :: (Integral a) => a -> [(a, Int)]
primeFactorization = primeFactorizationWithPrimes primes

factors :: (Integral a) => a -> [a]
factors k =
  let primeFactors = primeFactorization k
      powers a lastPower = lastPower : powers a (lastPower * a)
      firstNPowers a n = take (n + 1) (powers a 1)
      factorPowers = map (uncurry firstNPowers) primeFactors
   in foldr (\l1 l2 -> [a * b | b <- l2, a <- l1]) [1] factorPowers

factorSumWithPrimes :: (Integral a) => [a] -> a -> a
factorSumWithPrimes allPrimes k =
  let primeFactors = primeFactorizationWithPrimes allPrimes k
   in product $ map (\(a, n) -> ((a ^ (n + 1)) - 1) `div` (a - 1)) primeFactors

factorSum :: (Integral a) => a -> a
factorSum k =
  let primeFactors = primeFactorization k
   in product $ map (\(a, n) -> ((a ^ (n + 1)) - 1) `div` (a - 1)) primeFactors

allPrimesIn :: (Integral a) => [a] -> [a]
allPrimesIn = filter isPrime

primes :: (Integral a) => [a]
primes = 2 : 3 : 5 : filter isPrime [x + i | x <- [6, 12 ..], i <- [1, 5]]
