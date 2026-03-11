module Problem33 (solution) where

isTrivialY :: Int -> Bool
isTrivialY y = y `rem` 10 == 0

fracEqual :: (Int, Int) -> (Int, Int) -> Bool
fracEqual (x, y) (a, b) = x * b == y * a

qualifies :: (Int, Int) -> Bool
qualifies frac@(num, den) =
  let firstNum = num `div` 10
      secondNum = num `rem` 10
      firstDen = den `div` 10
      secondDen = den `rem` 10

      a1 = (firstNum == firstDen) && (frac `fracEqual` (secondNum, secondDen))
      a2 = (firstNum == secondDen) && (frac `fracEqual` (secondNum, firstDen))
      a3 = (secondNum == firstDen) && (frac `fracEqual` (firstNum, secondDen))
      a4 = (secondNum == secondDen) && (frac `fracEqual` (firstNum, firstDen))
   in (a1 || a2 || a3 || a4)

fractions :: [(Int, Int)]
fractions =
  [ (x, y)
    | y <- [11 :: Int .. 99],
      not (isTrivialY y),
      x <- [10 .. (y - 1)],
      qualifies (x, y)
  ]

lowestTermsDen :: Integer
lowestTermsDen =
  let numeratorProduct = product (map (fromIntegral . fst) fractions) :: Integer
      denominatorProduct = product (map (fromIntegral . snd) fractions) :: Integer
      common = gcd numeratorProduct denominatorProduct
   in (denominatorProduct `div` common)

solution :: IO Int
solution = pure (fromIntegral lowestTermsDen)
