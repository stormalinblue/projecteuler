module Problem2 (solution) where

import Util.Sequence (fibonacci)

solution :: IO Int
solution = return (sum $ filter even $ takeWhile (<= 4000000) fibonacci)
