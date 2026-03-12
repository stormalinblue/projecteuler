module Util.Data (getProblemDir, getProblemInput) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Paths_projecteuler (getDataFileName)

getProblemDir :: Int -> IO String
getProblemDir a = getDataFileName ("assets/" ++ show a)

getProblemInput :: Int -> IO Text
getProblemInput a = do
  filePath <- getDataFileName ("assets/" ++ show a ++ "/input.txt")
  TIO.readFile filePath
