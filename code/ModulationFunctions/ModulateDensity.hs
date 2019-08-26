{- |
Module      :  HypertextureLanguage.ModulationFunctions.ModulateDensity
Maintainer  :  sf16540@my.bristol.ac.uk

Useful function for 'ModulateDensity'.

-}

module ModulationFunctions.ModulateDensity (
  bias,
  gain,
) where

-- | Function that increases or decreases the objects density, controlled
--   by the value b. Controls mean. It should have the following properties:
--
-- * @ bias 0.5 t = t (b=0.5 should create the identity function) @
-- * @ bias b 0 = 0 (only when bias is between 0 and 1) @ 
-- * @ bias b 1 = 1 @
--
-- >>> bias 0.5 0.2
-- 0.2
--
-- >>> bias 0.7 0
-- 0.0
--
-- >>> bias 0.3 1
-- 1.0
--
bias
  :: Double -- ^ Bias value @(0<=b<1)@.
  -> Double -- ^ Incoming Density.
  -> Double -- ^ Modulated Density output.
bias b t = t ** ((log b) /  (log 0.5))

-- | Function that controls the steepness of the gradient, controlled
--   by the value g, making it flatter or steeper. It has these properties:
--
-- * @ gain g 0 = 0 @
-- * @ gain 0.25 g ≈ (1-g)/2 @
-- * @ gain g 0.5 = 0.5 @
-- * @ gain g 0.75 ≈ (g+1)/2 @
-- * @ gain g 1 = 1 @
-- * @ gain 0.5 t = t (b=0.5 should create the identity function) @
--
-- Examples:
--
-- >>> gain 0.4 0
-- 0.0
--
-- >>> gain 0.4 0.25
-- 0.3
--
-- >>> gain 0.7 0.5
-- 0.5
--
-- >>> gain 0.6  0.75
-- 0.8
--
-- >>> gain 0.2 1
-- 1.0
--
-- >>> gain 0.5  0.7
-- 0.7
--
gain
  :: Double -- ^ Gradient value @(0<g<=1)@.
  -> Double -- ^ Incoming Desnity.
  -> Double -- ^ Modulated Desnity output.
gain g t
  | t < 0.5   = (bias (2*t) (1-g)) / 2
  | otherwise = 1 - (bias (2-2*t) (1-g))/2