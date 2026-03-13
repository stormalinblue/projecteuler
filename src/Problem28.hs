module Problem28 (solution) where

corners :: [Int]
corners =
  let diffs = concat [[x, x, x, x] | x <- [2, 4 ..]]
   in scanl (+) 1 diffs

solution :: IO Int
solution = do
  return $ sum $ takeWhile (<= (1001 * 1001)) corners
