module Problem22 (solution, nameValue) where

import Data.Char (ord)
import Data.List (sort)
import Data.Text (Text, split, unpack)
import Util.Data (getProblemInput)
import Prelude hiding (lines, words)

alphaIndex :: Char -> Int
alphaIndex c = 1 + ord c - ord 'A'

nameValue :: String -> Int
nameValue s = sum $ map alphaIndex s

parseInput :: Text -> [String]
parseInput input =
  let words = unpack <$> split (== ',') input
      names = init . tail <$> words
   in names

pureSolution :: Text -> Int
pureSolution input = sum products
  where
    names = parseInput input
    products = zipWith (*) [1 ..] (map nameValue (sort names))

solution :: IO Int
solution = do
  input <- getProblemInput 22
  return $ pureSolution input
