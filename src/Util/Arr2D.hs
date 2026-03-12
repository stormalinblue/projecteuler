module Util.Arr2D (slice, Arr2D, toArr2D) where

import Data.Array.Repa (Array, DIM2, U, Z (..), fromListUnboxed, (!), (:.) (..))
import Data.Array.Repa.Repr.Unboxed (Unbox)

type Arr2D = Array U DIM2

toArr2D :: (Unbox e) => [[e]] -> Arr2D e
toArr2D xss =
  let rows = length xss
      cols = length (head xss)
      flat = concat xss
   in fromListUnboxed (Z :. rows :. cols) flat

slice :: (Unbox a) => [(Int, Int)] -> Arr2D a -> [a]
slice xs arr = [arr ! (Z :. i :. j) | (i, j) <- xs]
