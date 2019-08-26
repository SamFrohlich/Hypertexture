{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BooleanSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import DeepEmbedding.Boolean
import SetInstance
import Point
import Data.Fix

spec :: Spec
spec = do
  describe "Commutativity Laws" $ do
    prop "Union" $ 
      ((\a b -> (alg $ Union a b) == (alg $ Union b a)) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
    prop "Intersection" $ 
      ((\a b -> (alg $ Intersection a b) == (alg $ Intersection b a)) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
  describe "Associative Laws" $ do
    prop "Union" $ 
      ((\a b c -> (alg $ Union (alg $ Union a b) c) == (alg $ Union a (alg $ Union b c))) :: Fuzzy Point3D -> Fuzzy Point3D -> Fuzzy Point3D -> Bool)
    prop "Intersection" $ 
      ((\a b c -> (alg $ Intersection (alg $ Intersection a b) c) == (alg $ Intersection a (alg $ Intersection b c))) :: Fuzzy Point3D -> Fuzzy Point3D -> Fuzzy Point3D -> Bool)
  describe "Distributive Laws" $ do
    prop "Intersection then union" $ 
      ((\a b c -> (alg $ Union a (alg $ Intersection b c)) == (alg $ Intersection (alg $ Union a b) (alg $ Union a c))) :: Fuzzy Point3D -> Fuzzy Point3D -> Fuzzy Point3D -> Bool)
    prop "Union then intersection" $ 
      ((\a b c -> (alg $ Intersection a (alg $ Union b c)) == (alg $ Union (alg $ Intersection a b) (alg $ Intersection a c))) :: Fuzzy Point3D -> Fuzzy Point3D -> Fuzzy Point3D -> Bool)
  describe "Identity Laws" $ do
    prop "Union" $ 
      ((\a -> (alg $ Union a (alg $ Empty)) == a) :: Fuzzy Point3D -> Bool)
    prop "Intersection" $ 
      ((\a -> (alg $ Intersection a (alg $ Universal)) == a) :: Fuzzy Point3D -> Bool)
  describe "Idempotent Laws" $ do
    prop "Union" $ 
      ((\a -> (alg $ Union a a) == a) :: Fuzzy Point3D -> Bool)
    prop "Intersection" $
      ((\a -> (alg $ Union a a) == a) :: Fuzzy Point3D -> Bool)
  describe "Domination Laws" $ do
    prop "Union" $ 
      ((\a -> (alg $ Union a (alg $ Universal)) == (alg $ Universal)) :: Fuzzy Point3D -> Bool)
    prop "Intersection" $ 
      ((\a -> (alg $ Intersection a (alg $ Empty)) == (alg $ Empty)) :: Fuzzy Point3D -> Bool)
  describe "Absorption Laws" $ do
    prop "Intersection then Union" $ 
      ((\a b -> (alg $ Union a (alg $ Intersection a b)) == a) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
    prop "Union then Intersection" $ 
      ((\a b -> (alg $ Intersection a (alg $ Union a b)) == a) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
  describe "De Morgan's Laws" $ do
    prop "Union" $ 
      ((\a b -> ((alg $ Complement (alg $ Union a b)) == (alg $ Intersection (alg $ Complement a) (alg $ Complement b)))) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
    prop "Itersection" $ 
      ((\a b -> ((alg $ Complement (alg $ Intersection a b)) == (alg $ Union (alg $ Complement a) (alg $ Complement b)))) :: Fuzzy Point3D -> Fuzzy Point3D -> Bool)
  describe "Involution law" $ 
    prop "Property test" $ 
      ((\a -> (alg $ Complement (alg $ Complement a)) == a) :: Fuzzy Point3D -> Bool)
  describe "Complement laws" $ do
    prop "Complement of Empty" $ 
      ((alg $ Complement (alg $ Empty))== (alg $ Universal :: Fuzzy Point3D))
    prop "Complement of Universal" $ 
      ((alg $ Complement (alg $ Universal))== (alg $ Empty :: Fuzzy Point3D))
    
instance Show (Fuzzy Point3D) where
  show x = "f"

instance Eq (Fuzzy Point3D) where
  (==) a b = withinRegion a == withinRegion b

withinRegion :: Fuzzy Point3D -> [(Point3D,LimitedRange)]
withinRegion s = [(P (x,y,z), (LR $ characteristicFunction s $ P (x,y,z)))  | x <- [1..10::Double] -- NOTE:- this range could be increased for more thorough testing
                                                                            , y <- [1..10::Double]
                                                                            , z <- [1..10::Double]]

newtype LimitedRange = LR Double deriving (Show,Eq)

instance Arbitrary (Fuzzy Point3D) where
  arbitrary = do
    (LR d) <- arbitrary
    return (createFuzzy (\x->d))

instance Arbitrary LimitedRange where
  arbitrary = do
    x <- choose (0,1)
    return (LR x)

instance Eq Point3D where
  (==) (P a) (P b) = a == b
