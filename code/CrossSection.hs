{- |
Module      :  HypertextureLanguage.CrossSection
Maintainer  :  sf16540@my.bristol.ac.uk

Will use the 'SetInstance' of the langauge to produce cross sections.

-}

{-# LANGUAGE PackageImports #-}

module CrossSection (
  ImageArray(ImageArray),
  writePGM,
  drawHypertexture,
) where

import DeepEmbedding
import SetInstance

import Data.Fix
import Numeric.Extra
import qualified "matrix" Data.Matrix as M

-- | Stores an array of pixel values as 'Double's.
newtype ImageArray = ImageArray [Double]

{- | Takes an 'ImageArray' and writes it to a @.pgm@ file.

Structure of a @.pgm@ file:

@
P2   # Sets it to grayscale mode
24 7 # Resolution
15   # Max value of pixels, 0 = black, max = white
# Then the array of pixels:
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
0  3  3  3  3  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0 15  0
0  3  3  3  0  0  0  7  7  7  0  0  0 11 11 11  0  0  0 15 15 15 15  0
0  3  0  0  0  0  0  7  0  0  0  0  0 11  0  0  0  0  0 15  0  0  0  0
0  3  0  0  0  0  0  7  7  7  7  0  0 11 11 11 11  0  0 15  0  0  0  0
0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
@
-}
writePGM 
  :: Int        -- ^ Width.
  -> Int        -- ^ Height.
  -> ImageArray -- ^ Pixels of 'Hypertexture'.
  -> IO ()      -- ^ Outputs to file
writePGM w h i = writeFile "crossSection.pgm" (toPGM i)
  where
    toPGM :: ImageArray -> String
    toPGM (ImageArray ps) = concat ["P2\n",(show w )," ", (show h),"\n1000\n", (concatMap toGrayScale ps)]
    toGrayScale :: Double -> String
    toGrayScale d = show (floor $ 1000*d) ++ " "

-- | Draws a cross section of the given 'Fuzzy' set hypertexture onto an
--  'ImageArray'.
drawHypertexture
  :: Int             -- ^ Width.
  -> Int             -- ^ Height
  -> Double          -- ^ Z value.
  -> Fuzzy (Point3D) -- ^ Hypertexture to draw as a 'Fuzzy' set.
  -> ImageArray
drawHypertexture w h z f = ImageArray $ fmap (\(x,y)-> characteristicFunction f (P (x,y,z))) pointsToCheck
  where
    pointsToCheck = [(x,y) | x <- [-minx..maxx], y <- [-miny..maxy]]
    minx = intToDouble $ w `div` 2 -1
    maxx = intToDouble $ w `div` 2
    miny = intToDouble $ h `div` 2 -1
    maxy = intToDouble $ h `div` 2