{-# LANGUAGE InstanceSigs #-}
module Util.Numbers
  ( divisible,
    isPrime,
    allPrimesIn,
    factorial,
    Digits (..),
    singleDigits,
    glueDigits
  )
where

divisible :: (Integral a) => a -> a -> Bool
divisible a b =
  a `rem` b == 0

isPrime :: (Integral a) => a -> Bool
isPrime a
  | a <= 1 = False
  | otherwise = null [x | x <- takeWhile (\n -> n * n <= a) [2 ..], divisible a x]

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