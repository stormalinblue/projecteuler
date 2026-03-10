module Problem40 (
    champernowneDigits,
    rawSolution,
    solution
)
where

champernowneDigits :: [Char]
champernowneDigits =
    concatMap show [(1::Int)..]

toNum :: Char -> Int
toNum '1' = 1
toNum '2' = 2
toNum '3' = 3
toNum '4' = 4
toNum '5' = 5
toNum '6' = 6
toNum '7' = 7
toNum '8' = 8
toNum '9' = 9
toNum '0' = 0
toNum _ = error "Unrecognized"

rawSolution :: [Int]
rawSolution =  [toNum (champernowneDigits !! (i - 1)) | i <- [1, 10, 100, 1000, 10000, 100000, 1000000]]

solution :: Int
solution = product rawSolution