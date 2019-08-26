{- |
Module      :  HypertextureLanguage.Point
Maintainer  :  sf16540@my.bristol.ac.uk

Type for points.

-}

{-# LANGUAGE PackageImports #-}

module Point (
  Point,
  newPoint,
  getX,
  getY,
  getZ
) where

import qualified "matrix" Data.Matrix as M

type Point = M.Matrix Double

-- | Returns the x coordinate of a 'Point'.
--
-- >>> getX (newPoint 9 3 5)
-- 9.0
--
getX :: Point -> Double
getX p = p M.! (1,1)

-- | Returns the y coordinate of a 'Point'.
--
-- >>> getY (newPoint 9 3 5)
-- 3.0
--
getY :: Point -> Double
getY p = p M.! (2,1)

-- | Returns the z coordinate of a 'Point'.
--
-- >>> getZ (newPoint 9 3 5)
-- 5.0
--
getZ :: Point -> Double
getZ p = p M.! (3,1)

-- | Function to make a 'Point'.
newPoint
  :: Double -- ^ x.
  -> Double -- ^ y.
  -> Double -- ^ z.
  -> Point  -- ^ Point.
newPoint x y z = M.fromList 3 1 [x,y,z]