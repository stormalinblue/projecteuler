module Problem28 (solution, coins, numChange) where

numChange :: Int -> [Int] -> Int
numChange 0 _ = 1
numChange _ [] = 0
numChange amt [coin] = if amt `rem` coin == 0 then 1 else 0
numChange amt (coin : rcoins) =
  let coinAmts = takeWhile (>= 0) (iterate (\x -> x - coin) amt)
      useCoin = sum $ map (`numChange` rcoins) coinAmts
   in useCoin

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

solution :: IO Int
solution = pure (numChange 200 coins)
