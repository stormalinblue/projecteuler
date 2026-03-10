module Problem37 (solution, rawSolution, leftTruncPrimes, rightTruncPrimes) where

import Data.List.Ordered (isect)
import Util.Numbers (allPrimesIn)

{- rightTruncWithDigits :: Int -> [Int]
rightTruncWithDigits 1 = [2, 3, 5, 7]
rightTruncWithDigits n =
  allPrimesIn [x * 10 + d | x <- rightTruncWithDigits (n - 1), d <- [1, 3, 5, 7, 9]] -}

rightTruncStep :: (Integral a, Integral b) => (b, [a]) -> (b, [a])
rightTruncStep (n, xs) =
  (n + 1, allPrimesIn [x * 10 + d | x <- xs, d <- [1, 3, 7, 9]])
{- 
leftTruncWithDigits :: Int -> [Int]
leftTruncWithDigits 1 = [2, 3, 5, 7]
leftTruncWithDigits n =
  allPrimesIn [x + d * 10 ^ (n - 1) | d <- [1 .. 9], x <- leftTruncWithDigits (n - 1)] -}

leftTruncStep :: (Integral a, Integral b) => (b, [a]) -> (b, [a])
leftTruncStep (n, xs) =
  (n + 1, allPrimesIn [x + d * 10 ^ (n - 1) | d <- [1 .. 9], x <- xs])

leftTruncPrimes :: [Int]
leftTruncPrimes = concat $ takeWhile (not . null) (map snd (iterate leftTruncStep (2 :: Int, [2, 3, 5, 7])))

rightTruncPrimes :: [Int]
rightTruncPrimes = concat $ takeWhile (not . null) (map snd (iterate rightTruncStep (2 :: Int, [2, 3, 5, 7])))

rawSolution :: [Int]
rawSolution = let
    relRightTruncPrimes = dropWhile (< 10) rightTruncPrimes
    maxRightTruncPrime = maximum relRightTruncPrimes
    relLeftTruncPrimes = (dropWhile (< 10) . takeWhile (<= maxRightTruncPrime)) leftTruncPrimes
    in isect relLeftTruncPrimes relRightTruncPrimes

solution :: Int
solution = sum rawSolution
