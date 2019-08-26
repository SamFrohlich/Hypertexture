{-# LANGUAGE PackageImports #-}

module PointSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Point
import qualified "matrix" Data.Matrix as M

spec :: Spec
spec = do
    describe "Testing newPoint" $ do
      it "Example 1" $ do
        (newPoint 1 2 3) `shouldBe` M.fromList 3 1 [1,2,3]
      it "Example 2" $ do
        (newPoint 11 22 33) `shouldBe` M.fromList 3 1 [11,22,33]
      it "Example 2" $ do
        (newPoint 12 34 56) `shouldBe` M.fromList 3 1 [12,34,56]
    describe "Testing getX" $ do
      it "Example 1" $ do
        (getX (newPoint 1 2 3)) `shouldBe` 1
      it "Example 2" $ do
        (getX (newPoint 2 4 5)) `shouldBe` 2
      it "Example 3" $ do
        (getX (newPoint 12 34 56)) `shouldBe` 12
    describe "Testing getY" $ do
      it "Example 1" $ do
        (getY (newPoint 1 2 3)) `shouldBe` 2
      it "Example 2" $ do
        (getY (newPoint 2 4 5)) `shouldBe` 4
      it "Example 3" $ do
        (getY (newPoint 12 34 56)) `shouldBe` 34
    describe "Testing getZ" $ do
      it "Example 1" $ do
        (getZ (newPoint 1 2 3)) `shouldBe` 3
      it "Example 2" $ do
        (getZ (newPoint 2 4 5)) `shouldBe` 5
      it "Example 3" $ do
        (getZ (newPoint 12 34 56)) `shouldBe` 56
