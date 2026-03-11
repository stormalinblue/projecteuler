module Problem24 (solution) where

import Util.Numbers (glueDigits)
import Prelude hiding (seq)

million :: Int
million = 1000000

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

nthPerm :: Int -> [a] -> [a]
nthPerm 0 seq = seq
nthPerm _ [] = []
nthPerm n seq =
  let f = factorial (length seq - 1)
      p = n `div` f
      r = n `rem` f

      (before, a@(pos : after)) = splitAt p seq
   in pos : nthPerm r (before ++ after)

oneBasedNthPerm :: Int -> [a] -> [a]
oneBasedNthPerm i = nthPerm (i - 1)

solution :: IO Int
solution = pure $ fromIntegral $ glueDigits (oneBasedNthPerm million [0 .. 9])
