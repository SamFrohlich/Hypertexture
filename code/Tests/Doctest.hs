module Main (main) where

  import Test.DocTest (doctest)
    
  main :: IO ()
  main = doctest ["ModulationFunctions/ModulateDensity.hs", "ModulationFunctions/Noise.hs", "Point.hs"]