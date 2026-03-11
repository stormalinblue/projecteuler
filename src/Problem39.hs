module Problem39 (solution) where

{-
    Assuming (I haven't proved it myself) that Pythagorean triplet trees only grow the perimeter,
    we can prune these trees and get a list of candidate primitive triplets which may be factors
    of triples which have perimeter p.
-}

import Data.List (maximumBy)

filteredPrimitiveTriplets :: ((Int, Int, Int) -> Bool) -> [(Int, Int, Int)]
filteredPrimitiveTriplets f =
  let mulA (a, b, c) = (a - 2 * b + 2 * c, 2 * a - b + 2 * c, 2 * a - 2 * b + 3 * c)

      mulB (a, b, c) = (a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c)

      mulC (a, b, c) = (-a + 2 * b + 2 * c, -(2 * a) + b + 2 * c, -(2 * a) + 2 * b + 3 * c)

      generate triplet =
        [mulA triplet, mulC triplet, mulB triplet]
   in concat (takeWhile (not . null) (iterate (concatMap (filter f . generate)) [(3, 4, 5)]))

allPrimitiveTriplets :: [(Int, Int, Int)]
allPrimitiveTriplets = filteredPrimitiveTriplets $ const True

primitiveTriplesBelow :: Int -> [(Int, Int, Int)]
primitiveTriplesBelow n = filteredPrimitiveTriplets $ (\(a, b, c) -> (a + b + c) <= n)

countAllSolutions :: [Int] -> [Int]
countAllSolutions xs =
  let perimeters = map (\(a, b, c) -> a + b + c) (primitiveTriplesBelow 1000)
      countSolutions per p = length (filter (\x -> p `rem` x == 0) per)
   in map (countSolutions perimeters) xs

solution :: IO Int
solution = pure (fst (maximumBy (\x y -> compare (snd x) (snd y)) (zip [1 .. 1000] $ countAllSolutions [1 .. 1000])))
