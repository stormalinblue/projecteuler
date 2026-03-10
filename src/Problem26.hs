module Problem26 (solution, recurringCycles, modPowers) where
import Data.Foldable (maximumBy)
import Data.Function (on)

modPowers :: Int -> Int -> [Int]
modPowers n m =
    iterate (\x -> (x * n) `rem` m) 1

recurringCycles :: Int -> Int
recurringCycles n
    | n == 1 = 0
    | even n = recurringCycles (n `div` 2)
    | n `rem` 5 == 0 = recurringCycles (n `div` 5)
    | otherwise = 1 + length (takeWhile (/= 1) $ tail (modPowers 10 n))

solution :: Int
solution = maximumBy (compare `on` recurringCycles) [1..999]