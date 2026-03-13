module Problem3 (solution) where

import Util.Primes (primeFactorization)

constant :: Int
constant = 600851475143

solution :: IO Int
solution = do
  pure $ (fst . last . primeFactorization) constant
