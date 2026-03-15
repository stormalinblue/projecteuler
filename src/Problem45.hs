module Problem45 (solution) where

import Data.List.Ordered (isect)

triangle :: Int -> Int
triangle n = (n * (n + 1)) `div` 2

triangles :: [Int]
triangles = map triangle [1 ..]

pentagon :: Int -> Int
pentagon n = (n * (3 * n - 1)) `div` 2

pentagons :: [Int]
pentagons = map pentagon [1 ..]

hexagon :: Int -> Int
hexagon n = n * (2 * n - 1)

hexagons :: [Int]
hexagons = map hexagon [1 ..]

pureSolution :: Int
pureSolution = head $ dropWhile (<= 40755) (triangles `isect` pentagons `isect` hexagons)

solution :: IO Int
solution = do
  pure pureSolution
