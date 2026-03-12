{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Map as Map
-- where loadNumberedMap is defined

import qualified Problem1
import qualified Problem11
import qualified Problem19
import qualified Problem24
import qualified Problem26
import qualified Problem27
import qualified Problem28
import qualified Problem29
import qualified Problem33
import qualified Problem34
import qualified Problem37
import qualified Problem39
import qualified Problem40
import qualified Problem46
import ProblemLoader
import System.Environment (getArgs)

-- Automatically generate:
--   import qualified ProblemX
--   Map.fromList [(1, Problem1.solution), (2, Problem2.solution), ...]
solutions :: Map.Map Int (IO Int)
solutions =
  $( loadNumberedMap
       [1, 11, 19, 24, 26, 27, 28, 29, 33, 34, 37, 39, 40, 46]
   )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nStr] ->
      case reads nStr of
        [(n, "")] ->
          case Map.lookup n solutions of
            Just sol -> sol >>= print
            Nothing -> putStrLn "No such problem number."
        _ -> putStrLn "Argument must be an integer."
    _ -> putStrLn "Usage: program <problem-number>"
