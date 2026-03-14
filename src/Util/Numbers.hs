module Util.Numbers
  ( divisible,
    factorial,
    Digits (..),
    singleDigits,
  )
where

divisible :: (Integral a) => a -> a -> Bool
divisible a b =
  a `rem` b == 0

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

class (Integral a) => Digits a where
  revDigits :: a -> [Int]
  digits :: a -> [Int]
  digits = reverse . revDigits

  glueDigits :: [Int] -> a

instance Digits Integer where
  revDigits 0 = [0]
  revDigits n =
    let revDigitsPos 0 = []
        revDigitsPos x = fromInteger (x `rem` 10) : revDigitsPos (x `div` 10)
     in revDigitsPos n

  glueDigits = foldl (\x y -> x * 10 + toInteger y) 0

instance Digits Int where
  revDigits 0 = [0]
  revDigits n =
    let revDigitsPos 0 = []
        revDigitsPos x = x `rem` 10 : revDigitsPos (x `div` 10)
     in revDigitsPos n

  glueDigits = foldl (\x y -> x * 10 + y) 0

singleDigits :: [Int]
singleDigits = [0 .. 9]
