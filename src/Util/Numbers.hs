{-# LANGUAGE InstanceSigs #-}

module Util.Numbers
  ( divisible,
    isPrime,
    allPrimesIn,
    factorial,
    primeFactorization,
    Digits (..),
    singleDigits,
    glueDigits,
  )
where

divisible :: (Integral a) => a -> a -> Bool
divisible a b =
  a `rem` b == 0

isPrime :: (Integral a) => a -> Bool
isPrime a
  | a <= 1 = False
  | otherwise = null [x | x <- takeWhile (\n -> n * n <= a) [2 ..], divisible a x]

primes :: (Integral a) => [a]
primes = filter isPrime [1 ..]

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

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

class (Integral a) => Digits a where
  revDigits :: a -> [Int]
  digits :: a -> [Int]
  digits = reverse . revDigits

instance Digits Integer where
  revDigits :: Integer -> [Int]
  revDigits 0 = [0]
  revDigits n =
    let revDigitsPos 0 = []
        revDigitsPos x = fromInteger (x `rem` 10) : revDigitsPos (x `div` 10)
     in revDigitsPos n

instance Digits Int where
  revDigits :: Int -> [Int]
  revDigits 0 = [0]
  revDigits n =
    let revDigitsPos 0 = []
        revDigitsPos x = x `rem` 10 : revDigitsPos (x `div` 10)
     in revDigitsPos n

singleDigits :: [Int]
singleDigits = [0 .. 9]

glueDigits :: [Int] -> Integer
glueDigits = foldl (\x y -> x * 10 + toInteger y) 0
