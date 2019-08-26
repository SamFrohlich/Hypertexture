{- |
Module      :  HypertextureLanguage.DeepEmbedding.Transformation
Maintainer  :  sf16540@my.bristol.ac.uk

Language for descibing the affine transformations of 2D and 3D shapes.
Has the semantic model of sets of 2D or 3D points.

Creates a new characteristic function for the set by reversing the tranformation 
on the point then using the pre-existing characteristic function to see 
if it is part of the shape.

-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveFunctor  #-}

module DeepEmbedding.Transformation (
  Transformation(Translate, Rotate, Scale)
) where

import qualified "matrix" Data.Matrix as M

-- | Descibes possible affine tranformations.
data Transformation k 
  -- | Translates shape @k@ by translation vector @t@:
  --
  -- @ \\p . k (p-t) @
  = Translate (M.Matrix Double) k
  -- | Rotates shape @k@ by rotation matrix @r@:
  --
  -- @ \\p . k (r^(-1) p) @
  | Rotate (M.Matrix Double) k
    -- | Scales shape @k@ by scalar @s@:
    --
    -- @ \\p . k (s k) @
  | Scale Double k
  deriving (Show, Functor)