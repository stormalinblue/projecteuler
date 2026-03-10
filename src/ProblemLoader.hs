{-# LANGUAGE TemplateHaskell #-}

module ProblemLoader (loadNumberedMap) where

import qualified Data.Map
import Language.Haskell.TH

loadNumberedMap :: [Int] -> Q Exp
loadNumberedMap nums = do
  let mkModName n = "Problem" ++ show n
  pairs <-
    mapM
      ( \n -> do
          let modName = mkModName n
          keyExp <- litE (IntegerL (fromIntegral n))
          valExp <- varE (mkName (modName ++ ".solution"))
          pure (tupE [pure keyExp, pure valExp])
      )
      nums
  [|(Data.Map.fromList $(listE pairs))|]
