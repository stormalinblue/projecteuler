module Problem34
  ( sumDigitFactorial,
    uniqDigitCombinations,
    rawSolution,
    solution,
  )
where

import qualified Data.Map as Map
import Util.Numbers (Digits (..), digits, singleDigits)

digitFact :: Int -> Int
digitFact 0 = 1
digitFact 1 = 1
digitFact 2 = 2
digitFact 3 = 6
digitFact 4 = 24
digitFact 5 = 120
digitFact 6 = 720
digitFact 7 = 5040
digitFact 8 = 40320
digitFact 9 = 362880
digitFact _ = error "Digit must be between 0 and 9 inclusive"

uniqDigitCombinations :: Int -> [[Int]]
uniqDigitCombinations 1 = [[d] | d <- singleDigits]
uniqDigitCombinations n =
  let prevDigits = uniqDigitCombinations (n - 1)
      newCombos = [x : c | c <- prevDigits, x <- [head c .. 9]]
   in newCombos

sumDigitFactorial :: Int -> Int
sumDigitFactorial = sum . map digitFact . revDigits

rawSolution :: [Int]
rawSolution =
  let combos = concatMap uniqDigitCombinations [1 .. 15]
      sumFactorialDigits = (digits . sum . map digitFact)
      countDigits :: [Int] -> Map.Map Int Int
      countDigits = Map.fromListWith (+) . map (\x -> (x, 1))
      f :: [Int] -> Bool
      f c = countDigits c == countDigits (sumFactorialDigits c)
   in dropWhile (<= 2) $ map (sum . map digitFact) (filter f combos)

solution :: Int
solution = sum rawSolution
