{- |
Module      :  HypertextureLanguage.DeepEmbedding.Shapes2D
Maintainer  :  sf16540@my.bristol.ac.uk

Language for describing 2D shapes with a mathematical model:

@ f(x,y) @

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor     #-}

module DeepEmbedding.Shapes2D (
  Prim2D(Circle, Triangle, Square),
  Implicit2D(Implicit2D)
) where

-- | Primative 2D shapes. 
data Prim2D k 
  -- | Unit circle around the origin:
  --
  -- @ sqrt(x^2+y^2) - 1 @
  = Circle
  -- | Unit right angled triangle, right angle at origin:
  --
  -- @ x+y-1 @
  | Triangle
  -- | Square of side length 2 about origin:
  --
  -- @ |x|+|y|-1 @
  | Square
  deriving (Show, Functor)

-- | Adds ability to include custom shapes via an implicit function.
data Implicit2D k 
  -- | Takes a function @f@ of @x@ and @y@
  --
  -- @ f @
  = Implicit2D (Double -> Double -> Double)
  deriving (Show, Functor)

instance Show (Double -> Double -> Double) where
  show x = "f"