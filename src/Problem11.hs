module Problem11 (solution) where

import Data.Text (Text, lines, strip, unpack, words)
import Util.Arr2D
import Util.Data (getProblemInput)
import Prelude hiding (lines, words)

parseInput :: Text -> Arr2D Int
parseInput a =
  let splitWords :: Text -> [Text]
      splitWords = words . strip
      lineMap :: [Text] -> [Int]
      lineMap = map (read . unpack)
      splitLines :: Text -> [Text]
      splitLines = lines . strip
   in toArr2D $ map (lineMap . splitWords) (splitLines a)

cols :: Int -> Int -> [[(Int, Int)]]
cols numRows numCols =
  [ [(i, j) | i <- [row .. row + 3]] | j <- [0 .. numRows - 1], row <- [0 .. numCols - 4]
  ]

rows :: Int -> Int -> [[(Int, Int)]]
rows numRows numCols = [[(j, i) | (i, j) <- batch] | batch <- cols numCols numRows]

mainDiags :: Int -> Int -> [[(Int, Int)]]
mainDiags numRows numCols =
  [ [ (rowOffset + i, colOffset + i)
      | i <- [0 .. 3]
    ]
    | rowOffset <- [0 .. numRows - 4],
      colOffset <- [0 .. numCols - 4]
  ]

crossDiags :: Int -> Int -> [[(Int, Int)]]
crossDiags numRows numCols = [[(numRows - 1 - i, j) | (i, j) <- batch] | batch <- mainDiags numRows numCols]

listProd :: [(Int, Int)] -> Arr2D Int -> Int
listProd xs arr = product $ slice xs arr

prods :: Arr2D Int -> [Int]
prods arr =
  let funcs :: [Int -> Int -> [[(Int, Int)]]]
      funcs = [cols, rows, mainDiags, crossDiags]
      groups = concatMap (\f -> f 20 20) funcs
      result = map (`listProd` arr) groups
   in result

solution :: IO Int
solution = do
  input <- getProblemInput 11
  let a = parseInput input
  pure (maximum $ prods a)
