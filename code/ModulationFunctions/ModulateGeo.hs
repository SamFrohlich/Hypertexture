{- |
Module      :  HypertextureLanguage.ModulationFunctions.ModulateGeo
Maintainer  :  sf16540@my.bristol.ac.uk

Useful function for 'ModulateGeometry'.

-}

{-# LANGUAGE PackageImports #-}

module ModulationFunctions.ModulateGeo (
  softHalo,
  hair,
  hairSettings,
) where

import DeepEmbedding
import ModulationFunctions.Noise
import ModulationFunctions.ModulateDensity
import Point
import qualified "matrix" Data.Matrix as M
import qualified Data.List as L

-- | Adds a halo of soft around a a 'Hypertexture' so that 'hair' can be added.
softHalo
  :: Hypertexture 
  -> Hypertexture
softHalo s = union s (scale 2 (constSoften 0.3 s))

-- | Takes an Object Density Function and creates a list of all points 
--   belonging to the hard region of 'Hypertexture'.
findHard
  :: (Point -> Double) -- ^ ODF.
  -> [Point]           -- ^ Region to consider
  -> [Point]           -- ^ Points belonging to the hard region
findHard f = filter (\p-> f p == 1)

-- | Converts a point in the soft region to its nearest hard region point.
project
  :: [Point] -- ^ List of points in hard region
  -> Point   -- ^ Point to convert
  -> Point   -- ^ Equivalent point in hard region
project ps p = fst $ L.minimumBy (\(_,d1)(_,d2)->compare d1 d2) $ fmap (\p1-> (p1,euclideanDist p p1)) ps

-- | Calculates the Euclidean distance between two 'Point's.
euclideanDist :: Point -> Point -> Double
euclideanDist p1 p2 = sqrt $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2
  where
    x1  = p1 M.! (1,1)
    y1  = p1 M.! (2,1)
    z1  = p1 M.! (3,1)
    x2  = p2 M.! (1,1)
    y2  = p2 M.! (2,1)
    z2  = p2 M.! (3,1)

-- | Function to simulate hair/fur.
hair
  :: NoiseSettings     -- ^ Desired settings for noise.
  -> (Double,Double) -- ^ x Range. (min,max).
  -> (Double,Double) -- ^ y Range. (min,max).
  -> (Double,Double) -- ^ z Range. (min,max).
  -> [Point]           -- ^ Region to consider.
  -> Double            -- ^ Frequency.
  -> (Point -> Double) -- ^ ODF.
  -> Point             -- ^ Point to find density at.
  -> Double            -- ^ Density.
hair settings (minx,maxx) (miny,maxy) (minz,maxz) r f odf p =
  let
    hardPoints = findHard odf r
    pp = project hardPoints p
    px  = pp M.! (1,1)
    py  = pp M.! (2,1)
    pz  = pp M.! (3,1)
    scaledx = (px - minx) / (maxx+(abs minx))
    scaledy = (py - miny) / (maxy+(abs miny))
    scaledz = (pz - minz) / (maxz+(abs minz))
    fp = (M.fromList 3 3 [f,0,0,0,f,0,0,0,f]) * (M.fromList 3 1 [scaledx, scaledy, scaledz])
    n          = (noise settings fp)
    n'         = (bias 0.9 n)
    density    = odf p
  in (if (density /= 1 && density /= 0) then (n' * density) else density)

-- | Good 'NoiseSettings' for hair.
hairSettings = NoiseSettings  { gradients = perm
                              , steepness = 1
                              , squiggliness = 20
                              , inverted  = True
                              }   