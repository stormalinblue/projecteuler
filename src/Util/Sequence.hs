module Util.Sequence (fibonacci) where

fibonacci :: (Integral a) => [a]
fibonacci = fibInner 1 1
  where
    fibInner a b = a : fibInner b (a + b)
