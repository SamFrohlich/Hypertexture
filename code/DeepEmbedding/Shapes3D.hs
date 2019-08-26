{- |
Module      :  HypertextureLanguage.DeepEmbedding.Shapes3D
Maintainer  :  sf16540@my.bristol.ac.uk

Language for describing 3D shapes with a mathematical model:

 @ f(x,y,z) @

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor     #-}

module DeepEmbedding.Shapes3D (
  Prim3D(Cube, Cylinder, Sphere, Cone, Torus),
  Implicit3D(Implicit3D),
  Prism3D(Prism3D)
) where

-- | Primative 3D shapes.
data Prim3D k 
  -- | Cube centered around origin with sides of length 2:
  --
  -- @ max(|x|,|y|,|z|) - 1 @
  = Cube
  -- | Cylinder with radius 1 and height 1 with base centered on origin:
  --
  -- @ (sqrt(x^2+y^2) - 1) <=0 && z >=0 && z <=1 @
  | Cylinder
  -- | Unit sphere centered at origin:
  --
  -- @ x^2 + y^2 + z^2 - 1 @
  | Sphere
  -- | Cone with height 1 with base centered on origin:
  --
  -- @ x^2 + y^2 - (z-1)^2 && z >=0 && z <=1 @
  | Cone
  -- | Torus centered about origin with major radius = 1 and minor radius = 0.5:
  --
  -- @ (sqrt(x^2 + y^2) - 1)^2 + z^2 - 0.5 @
  | Torus
  deriving (Show, Functor)

-- | Adds ability to include custom shapes via an implicit function.
data Implicit3D k 
  -- | Takes a function @f@ of @x@, @y@ and @z@:
  --
  -- @ f @
  = Implicit3D (Double -> Double -> Double -> Double)
  deriving (Show, Functor)

instance Show (Double -> Double -> Double -> Double) where
  show x = "f"

-- | Lifts a 2D shape to 3D by adding height of 1 above and below in the z direction.
data Prism3D k
  -- | Lifts polygon @p@:
  --
  -- @ (p x y) + (|z| - 1) @
  = Prism3D k
  deriving (Show, Functor)