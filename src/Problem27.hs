module Problem27 (divisible, genPrimes, genQuadratic, rawSolution, solution) where

import Data.Function (on)
import Data.List (maximumBy)
import Util.Numbers (divisible)
import Util.Primes (isPrime)

genQuadratic :: (Integral a) => a -> a -> [a]
genQuadratic a b = [(x * x) + (x * a) + b | x <- [0 ..]]

genPrimes :: (Integral a) => a -> a -> [a]
genPrimes a b = takeWhile isPrime (genQuadratic a b)

longestBy :: (a -> [b]) -> [a] -> a
longestBy fn = maximumBy (compare `on` (length . fn))

rawSolution :: (Int, Int)
rawSolution = bestPair where bestPair = longestBy (uncurry genPrimes) [(x, y) | y <- filter isPrime [2 .. 1000], x <- filter (\x -> isPrime (1 + x + y)) [-y + 1 .. 999]]

solution :: IO Int
solution = pure (uncurry (*) rawSolution)
