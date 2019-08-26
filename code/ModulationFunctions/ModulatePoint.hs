{- |
Module      :  HypertextureLanguage.ModulationFunctions.ModulatePoint
Maintainer  :  sf16540@my.bristol.ac.uk

Useful function for 'ModulatePoint'.

-}

{-# LANGUAGE PackageImports #-}

module ModulationFunctions.ModulatePoint (
  dripX,
  dripY,
  dripZ,
  addNoise,
  addTurbulence,
  fire,
  fractalNoise,
  ) where

import ModulationFunctions.Noise
import Point
import qualified "matrix" Data.Matrix as M

-- | 'NoiseSettings' for 'fire' with a much greater 'squiggliness'.
fireNoiseSettings :: NoiseSettings
fireNoiseSettings = NoiseSettings  { gradients = perm
                                    , steepness = 1
                                    , squiggliness = 8
                                    , inverted  = True
                                    }

extractAndScale
  :: (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Point           -- ^ Point to scale and extract parts from.
  -> (Point,Double,Double,Double) -- ^ (scaled point, x, y, z)
extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
  = let 
      x  = getX p
      y  = getY p
      z  = getZ p
      scaledx = (x - minx) / (maxx+(abs minx))
      scaledy = (y - miny) / (maxy+(abs miny))
      scaledz = (z - minz) / (maxz+(abs minz))
      scaledPoint = (newPoint scaledx scaledy scaledz)
    in (scaledPoint, x, y, z)

-- | Will make a 'Hypertexture' drip in the x direction towards the x axis.
--   The further away from the axis it is the more it will drip. Ranges 
--   to scale points down to noise sample range.
dripX
  :: NoiseSettings   -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double           -- ^ Frequency of noise.
  -> Double           -- ^ Amplitude of noise.
  -> Point            -- ^ Point to change.
  -> Point            -- ^ Changed point.
dripX settings (minx,maxx) (miny,maxy) (minz,maxz) f a p = newPoint (n*x) y z
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * scaledPoint
    n  = (1+a*(noise settings fp))

-- | Will make a 'Hypertexture' drip in the y direction towards the y axis.
--   The further away from the axis it is the more it will drip. Ranges 
--   to scale points down to noise sample range.
dripY
  :: NoiseSettings   -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double           -- ^ Frequency of noise.
  -> Double           -- ^ Amplitude of noise.
  -> Point            -- ^ Point to change.
  -> Point            -- ^ Changed point.
dripY settings (minx,maxx) (miny,maxy) (minz,maxz) f a p = newPoint x (n*y) z
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * scaledPoint
    n  = (1+a*(noise settings fp))

-- | Will make a 'Hypertexture' drip in the z direction towards the z axis.
--   The further away from the axis it is the more it will drip. Ranges 
--   to scale points down to noise sample range.
dripZ
  :: NoiseSettings   -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double           -- ^ Frequency of noise.
  -> Double           -- ^ Amplitude of noise.
  -> Point            -- ^ Point to change.
  -> Point            -- ^ Changed point.
dripZ settings (minx,maxx) (miny,maxy) (minz,maxz) f a p = newPoint x y (n*z)
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * scaledPoint
    n  = (1+a*(noise settings fp))

-- | Adds noise to a 'Hypertexture'. Noise is drawn towards the origin.
--   For best results add noise at origin, then move to desired location.
addNoise
  :: NoiseSettings -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double        -- ^ Frequency of noise.
  -> Double        -- ^ Amplitude of noise.
  -> Point         -- ^ Point to change.
  -> Point         -- ^ Changed point.
addNoise settings (minx,maxx) (miny,maxy) (minz,maxz) f a p = newPoint (n*x) (n*y) (n*z)
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * scaledPoint
    n  = (1+a*(noise settings fp))

-- | Repeatedly adds noise to a hypertexture. Excellent for creating 'fire'.
addTurbulence
  :: NoiseSettings -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double        -- ^ Desired number of iterations.
  -> Point         -- ^ Point to change.
  -> Point         -- ^ Changed point.
addTurbulence settings (minx,maxx) (miny,maxy) (minz,maxz) i p = newPoint (n*x) (n*y) (n*z)
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    n  = (1+(turbulence settings i scaledPoint))

-- | Uses turbulence with 'NoiseSettings' optimised to create a good fire effect.
--   For best results transform afterwards.
fire 
  :: (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Point
  -> Point
fire rx ry rz = addTurbulence fireNoiseSettings rx ry rz 5

-- | Adds fractal noise to a 'Hypertexture'.
fractalNoise
  :: NoiseSettings -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double        -- ^ Desired number of iterations. 
  -> Double        -- ^ Frequency.
  -> Point         -- ^ Point to change.
  -> Point         -- ^ Changed point.
fractalNoise settings (minx,maxx) (miny,maxy) (minz,maxz) i f p = newPoint (n*x) (n*y) (n*z)
  where
    (scaledPoint,x,y,z) = extractAndScale (minx,maxx) (miny,maxy) (minz,maxz) p
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * scaledPoint
    n = (1 + 1/f * (turbulence settings i fp))