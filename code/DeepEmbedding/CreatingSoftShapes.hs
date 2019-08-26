{- |
Module      :  HypertextureLanguage.DeepEmbedding.CreatingSoftShapes
Maintainer  :  sf16540@my.bristol.ac.uk

Language for descibing soft shapes, either primatively soft, or lifted
from normal shapes. Has the semantic model of fuzzy sets of 2D or 3D Points.

-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveFunctor  #-}

module DeepEmbedding.CreatingSoftShapes (
  Soften(ConstSoften, FromPointSoften),
  PrimSoft(SoftSphere)
) where

import qualified "matrix" Data.Matrix as M

-- | Lifts normal shapes to soft shapes by changing their characteristic
--   function.
data Soften k
  -- | Uniformly softens all points in shape @k@ by value between @n@ 0-1.
  --
  -- @ \\p. n*(k p) @
  = ConstSoften Double k
  -- | Softens points based on how far they are away from a point @c@
  --   as a percentage of some maximum distance @r@.
  --
  -- @ \\p . (1-d\/r)*(k p) where d = euclideanDist p c @
  | FromPointSoften (M.Matrix Double) Double k
  deriving (Show, Functor)

-- | Primatives of soft shapes.
data PrimSoft k
  -- | Unit soft sphere around the origin:
  --
  -- @ \\p. if (d < 1 ) then 1 - d else 0 where d = euclideanDist p origin @
  = SoftSphere
  deriving (Show, Functor)