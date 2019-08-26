{-# LANGUAGE FlexibleContexts #-}

module ModulateDensitySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import ModulationFunctions.ModulateDensity

spec :: Spec
spec = do
    describe "Testing bias" $ do
      -- Numeric tests
      it "Example 1: Identity property" $
          (bias 0.5 0.4) `shouldBe` 0.4
      it "Example 2: Zero property" $
          (bias 0.8 0) `shouldBe` 0
      it "Example 3: One property" $
          (bias 0.1 1) `shouldBe` 1
      -- Property tests
      prop "Identity property" $ 
          (\(LR t) -> bias 0.5 t == t)
      prop "Zero property" $ 
          (\(LR b) -> bias b 0 == 0)
      prop "One property" $ 
          (\(LR b) -> bias b 1 == 1)
    describe "Testing gain" $ do
      -- Numeric tests
      it "Example 1: Identity property" $
        (gain 0.5 0.4) `shouldBe` 0.4
      it "Example 2: Zero property" $
        (gain 0.5 1) `shouldBe` 1
      it "Example 3: One property" $
        (gain 0.5 0.4) `shouldBe` 0.4
      it "Example 4: Quarter property" $
        (gain 0.6 0.25) `shouldBe` 0.2
      it "Example 5: Three Quarter property" $
        (gain 0.7 0.75) `shouldBe` 0.85
      -- Property tests
      prop "Identity property" $ 
          (\(LR t) -> (round' $ gain 0.5 t) == (round' t))
      prop "Zero property" $ 
          (\(LR g) -> gain g 0 == 0)
      prop "One property" $ 
          (\(LR g) -> gain g 1 == 1)

newtype LimitedRange = LR Double deriving Show

instance Arbitrary LimitedRange where
  arbitrary = do
    x <- choose (0,1)
    return (LR x)
  
round' :: Double -> Double
round' x =  (fromInteger $ round $ (x * (10^10))) / (10.0^^10)