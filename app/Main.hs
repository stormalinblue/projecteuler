{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Map as Map
-- where loadNumberedMap is defined

import qualified Problem1
import qualified Problem11
import qualified Problem19
import qualified Problem2
import qualified Problem22
import qualified Problem23
import qualified Problem24
import qualified Problem26
import qualified Problem27
import qualified Problem28
import qualified Problem29
import qualified Problem3
import qualified Problem30
import qualified Problem31
import qualified Problem32
import qualified Problem33
import qualified Problem34
import qualified Problem37
import qualified Problem39
import qualified Problem4
import qualified Problem40
import qualified Problem41
import qualified Problem45
import qualified Problem46
import qualified Problem47
import qualified Problem50
import ProblemLoader
import System.Environment (getArgs)

-- Automatically generate:
--   import qualified ProblemX
--   Map.fromList [(1, Problem1.solution), (2, Problem2.solution), ...]
solutions :: Map.Map Int (IO Int)
solutions =
  $( loadNumberedMap
       [1, 2, 3, 4, 11, 19, 22, 23, 24, 26, 27, 28, 29, 30, 31, 32, 33, 34, 37, 39, 40, 41, 45, 46, 47, 50]
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
