module Util.Primes (isPrime, primeFactorization, allPrimesIn, primes) where

import Util.Numbers (divisible)

isPrime :: (Integral a) => a -> Bool
isPrime a
  | a <= 1 = False
  | otherwise = null [x | x <- takeWhile (\n -> n * n <= a) [2 ..], divisible a x]

primeFactorization :: (Integral a) => a -> [(a, Int)]
primeFactorization x
  | x <= 1 = []
  | otherwise =
      let countDivides :: (Integral a) => a -> a -> Int -> (Int, a)
          countDivides b p accum
            | b `divisible` p =
                countDivides (b `div` p) p (1 + accum)
            | otherwise = (accum, b)

          pfInner :: (Integral a) => a -> [a] -> [(a, Int)]
          pfInner _ [] = error "Cannot happen"
          pfInner b (p : ps)
            | b < p = []
            | b `divisible` p =
                let (count, remaining) = countDivides b p 0
                 in (p, count) : pfInner remaining ps
            | otherwise = pfInner b ps
       in pfInner x primes

allPrimesIn :: (Integral a) => [a] -> [a]
allPrimesIn = filter isPrime

primes :: (Integral a) => [a]
primes = 2:allPrimesIn [3,5..]
