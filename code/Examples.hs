{- |
Module      :  HypertextureLanguage.Examples
Maintainer  :  sf16540@my.bristol.ac.uk

Examples of 'Hypertexture'.

-}

{-# LANGUAGE PackageImports #-}

module Examples (
  fireball,
  noisySphere,
  drippingSphere,
  fractalSphere,
  erodedCube,
  furryDonut
) where

import DeepEmbedding
import ModulationFunctions.ModulateDensity
import ModulationFunctions.ModulatePoint
import ModulationFunctions.ModulateGeo
import ModulationFunctions.Noise
import Point

import Data.Fix
import qualified "matrix" Data.Matrix as M

-- | Given a range within which the fireball will be will produce code
--   for a fireball.
fireball
  :: (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double          -- ^ Scale.
  -> Hypertexture
fireball rx ry rz s = modulatePoint 
                      (fire rx ry rz) 
                      (scale s $ softSphere)

-- | Creates a noisy sphere with the desired given attributes within the 
--   specified location.
noisySphere
  :: NoiseSettings   -- ^ Desired settings for 'noise'.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double          -- ^ Frequency of noise.
  -> Double          -- ^ Amplitude of noise.
  -> Double          -- ^ Scale.
  -> Hypertexture
noisySphere settings (minx,maxx) (miny,maxy) (minz,maxz) f a s
  = modulatePoint 
    (addNoise settings (minx,maxx) (miny,maxy) (minz,maxz) f a) 
    (scale s $ softSphere)

-- | Creates a dripping sphere with the desired given attributes within 
--   the specified location. Sphere drips in the y axis.
drippingSphere
  :: NoiseSettings   -- ^ Desired settings for 'noise'.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double          -- ^ Frequency of noise.
  -> Double          -- ^ Amplitude of noise.
  -> Double          -- ^ Scale.
  -> Double          -- ^ Scale of drip.
  -> Hypertexture
drippingSphere settings (minx,maxx) (miny,maxy) (minz,maxz) f a s d
  = translate (M.fromList 3 1 [s*d,0,0]) $ 
      modulatePoint 
        (dripX settings (minx,maxx) (miny,maxy) (minz,maxz) f a) 
        (scale s (translate (M.fromList 3 1 [-d,0,0]) softSphere))

-- | Creates a fractal sphere with the desired given attributes within 
--   the specified location.
fractalSphere
  :: NoiseSettings   -- ^ Desired settings for 'noise'.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double          -- ^ Desired number of iterations. 
  -> Double          -- ^ Frequency.
  -> Double          -- ^ Scale.
  -> Hypertexture
fractalSphere settings (minx,maxx) (miny,maxy) (minz,maxz) i f s
  = modulatePoint 
      (fractalNoise settings (minx,maxx) (miny,maxy) (minz,maxz) i f) 
      (scale s $ softSphere)

-- | Creates an eroded cube.
erodedCube
  :: NoiseSettings   -- ^ Desired settings for 'noise'.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> Double          -- ^ Desired number of iterations for erosion.
  -> Double          -- ^ Frequency of erosion.
  -> Double          -- ^ Scale
  -> Hypertexture
erodedCube settings (minx,maxx) (miny,maxy) (minz,maxz) i f s
  = intersection 
      (scale s $ cube) 
      (fractalSphere settings (minx,maxx) (miny,maxy) (minz,maxz) i f s)

-- | Creates an furry donut.
furryDonut
  :: (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> [Point]         -- ^ Region to consider.
  -> Double          -- Scale
  -> Hypertexture
furryDonut (minx,maxx) (miny,maxy) (minz,maxz) points s
  = modulateGeometry 
      (hair hairSettings (minx,maxx) (miny,maxy) (minz,maxz) points 10) 
      (scale s $ softHalo torus)