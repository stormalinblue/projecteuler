module Problem30 (solution) where

import Util.Numbers (digits)

isEqualDigitSum :: Int -> Bool
isEqualDigitSum x = x == sum (map (^ 5) $ digits x)

{- Determined by hand that the all solutions have to be less than 1 million -}
pureSolution :: Int
pureSolution = sum $ filter isEqualDigitSum [2 .. 1000000]

solution :: IO Int
solution = do
  pure pureSolution
