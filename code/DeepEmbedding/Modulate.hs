{- |
Module      :  HypertextureLanguage.DeepEmbedding.Modulate
Maintainer  :  sf16540@my.bristol.ac.uk

Language for manipluating soft shapes to get exciting hypertexture phenomina.

-}

{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor     #-}

module DeepEmbedding.Modulate (
  ModulateDensity(ModulateDensity),
  ModulatePoint(ModulatePoint),
  ModulateGeometry(ModulateGeometry)
) where

import qualified "matrix" Data.Matrix as M
import Point

-- | Adjusts the existing membership value of a point.
data ModulateDensity k
  -- | Takes a function @f@ from to transform membership of @k@:
  --
  -- @ f . k @
  = ModulateDensity (Double -> Double) k
  deriving (Show, Functor)

instance Show (Double->Double) where
  show x = "f"

-- | Adjusts a point before seeing its membership value.
data ModulatePoint k
  -- | Takes a function @f@ to transform point before testing membership of @k@:
  --
  -- @ k . f @
  = ModulatePoint (Point -> Point) k
  deriving (Show, Functor)

instance Show (Point -> Point) where
  show x = "f"

-- | Changes the membership function based on the existing members.
data ModulateGeometry k
  -- | Adjusts membership function of @k@ using @f@:
  --
  -- @ f (k) @
  = ModulateGeometry ((Point -> Double)->(Point -> Double)) k
  deriving (Show, Functor)

instance Show ((Point -> Double)->(Point -> Double)) where
  show x = "f"