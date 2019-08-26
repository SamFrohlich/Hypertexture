{- |
Module      :  HypertextureLanguage.SetInstance
Maintainer  :  sf16540@my.bristol.ac.uk

Instance of the 'Hypertexture' language which is the same as its semantic
meaning: sets of points.

Includes a Haskell description of sets where they are described by their
characteristc function. This is a fuzzy set where membership is in range
[0-1].
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PackageImports        #-}

module SetInstance (
  Point3D(P),
  -- * Fuzzy Set Type
  Fuzzy(characteristicFunction),
  Alg(alg),
  -- * Fuzzy Set Constructors
  emptyFuzzy,
  universalFuzzy,
  createFuzzy,
  -- * Fuzzy Set Booleans
  unionFuzzy,
  intersectionFuzzy,
  complementFuzzy,
  differenceFuzzy,
) where

import DeepEmbedding
import DeepEmbedding.Infrastructure
import DeepEmbedding.Shapes2D
import DeepEmbedding.Shapes3D
import DeepEmbedding.Transformation
import DeepEmbedding.Boolean
import DeepEmbedding.CreatingSoftShapes
import DeepEmbedding.Modulate

import Data.Either
import Data.Fix
import qualified "matrix" Data.Matrix as M
import Numeric.Extra

-- | Elements of the 'Fuzzy 'set will have this type.
newtype Point3D = P (Double,Double,Double) deriving Show

-- | Calculates the Euclidean distance between two 'Point3D's.
euclideanDist :: Point3D -> Point3D -> Double
euclideanDist (P (x1,y1,z1)) (P (x2,y2,z2)) = sqrt $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

--                          Fuzzy Set Type
-- ===================================================================

-- | Data type for 'Fuzzy' set that gives the user access to the 'characteristicFunction'.
data Fuzzy a = Fuzzy {
  -- | Characteristic function of set, taking a candiate element and returning its membership.
  characteristicFunction :: a -> Double
  }

--                         Fuzzy Set Constructors
-- ===================================================================

-- | Creates an empty fuzzy set.
emptyFuzzy :: Fuzzy a
emptyFuzzy = Fuzzy (const 0)

-- | Creates the fuzzy set that accepts everything.
universalFuzzy :: Fuzzy a
universalFuzzy = Fuzzy (const 1)

-- | Given a characteristic function will create a 'Fuzzy' set. The given
--   function will be limited so that only values in range [0,1] are
--   returned.
createFuzzy
  :: (a -> Double) -- ^ Characteristic Function. Should return 'Double' in range [0,1].
  -> Fuzzy a       -- ^ 'Fuzzy' set defined by given 'characteristicFunction'.
createFuzzy f = Fuzzy (\x -> min 1 (max 0 (f x)))

--                         Fuzzy Set Booleans
-- ===================================================================

-- | Unions two 'Fuzzy' sets.
unionFuzzy :: Fuzzy a -> Fuzzy a -> Fuzzy a
unionFuzzy a b = Fuzzy (\x -> max (characteristicFunction a x) (characteristicFunction b x))

-- | Intersects two 'Fuzzy' sets.
intersectionFuzzy :: Fuzzy a -> Fuzzy a -> Fuzzy a
intersectionFuzzy a b = Fuzzy (\x -> min (characteristicFunction a x) (characteristicFunction b x))

-- | Complements a 'Fuzzy' set.
complementFuzzy :: Fuzzy a -> Fuzzy a
complementFuzzy a = Fuzzy (\x -> 1 - (characteristicFunction a x))

-- | Finds the difference between two 'Fuzzy' sets.
differenceFuzzy :: Fuzzy a -> Fuzzy a -> Fuzzy a
differenceFuzzy a b = unionFuzzy a (complementFuzzy b)

--                         Fuzzy Set Instance
-- ===================================================================

-- 2D Shapes
-- ------------

-- | Converts 2D primatives ('Prim2D') to 'Fuzzy' set of 3D points.
--   2D shapes are assumed to lie at @z = 0@.
instance Alg Prim2D (Fuzzy Point3D) where
  alg :: Prim2D (Fuzzy Point3D) -> Fuzzy Point3D
  alg Circle      = createFuzzy (\(P (x,y,z)) -> if ((sqrt(x^2+y^2) - 1) <=0 && z == 0) then 1 else 0)
  alg Triangle    = createFuzzy (\(P (x,y,z)) -> if ((x+y-1) <=0 && z == 0 && x >=0 && y >= 0) then 1 else 0)
  alg Square      = createFuzzy (\(P (x,y,z)) -> if (((abs x)+(abs y)-1 ) <=0 && z == 0) then 1 else 0)

-- | Converts 2D implicits ('Implicit2D') to 'Fuzzy' set of 3D points.
--   2D shapes are assumed to lie at @z = 0@.
instance Alg Implicit2D (Fuzzy Point3D) where
  alg :: Implicit2D (Fuzzy Point3D) -> Fuzzy Point3D
  alg (Implicit2D f) = createFuzzy (\(P (x,y,z)) -> if ((f x y) <=0 && z == 0) then 1 else 0)

-- 3D Shapes
-- ------------

-- | Converts 3D primatives ('Prim3D') to 'Fuzzy' set of 3D points.
instance Alg Prim3D (Fuzzy Point3D) where
  alg :: Prim3D (Fuzzy Point3D) -> Fuzzy Point3D
  alg Cube     = createFuzzy (\(P (x,y,z)) -> if (((maximum [abs x, abs y, abs z]) - 1) <=0) then 1 else 0)
  alg Cylinder = createFuzzy (\(P (x,y,z)) -> if ((sqrt(x^2+y^2) - 1) <=0 && z >=0 && z <=1) then 1 else 0)
  alg Sphere   = createFuzzy (\(P (x,y,z)) -> if ((x^2 + y^2 + z^2 - 1)  <=0) then 1 else 0)
  alg Cone     = createFuzzy (\(P (x,y,z)) -> if ((x^2 + y^2 - (z-1)^2) <= 0) then 1 else 0)
  alg Torus    = createFuzzy (\(P (x,y,z)) -> if (((sqrt(x^2 + y^2) - 1)^2 + z^2 - 0.5) <= 0) then 1 else 0)

-- | Converts 3D implicits ('Implicit3D') to 'Fuzzy' set of 3D points.
instance Alg Implicit3D (Fuzzy Point3D) where
  alg :: Implicit3D (Fuzzy Point3D) -> Fuzzy Point3D
  alg (Implicit3D f) = createFuzzy (\(P (x,y,z)) -> if ((f x y z) <=0) then 1 else 0)

-- | Converts 3D prisms ('Prism3D') to 'Fuzzy' set of 3D points.
instance Alg Prism3D (Fuzzy Point3D) where
  alg :: Prism3D (Fuzzy Point3D) -> Fuzzy Point3D
  alg (Prism3D p) = createFuzzy (\(P (x,y,z)) -> if ((characteristicFunction p (P (x,y,0)) == 1)&& (abs z) - 1 <= 0) then 1 else 0)

-- Transformations
-- ----------------

-- | Turns a 'M.Matrix' into a 'Point3D'.
matrixToPoint3D
  :: M.Matrix Double -- ^ Matrix to turn into point (should be correct size). 
  -> Point3D         -- ^ Converted point.
matrixToPoint3D m = P (x,y,z)
  where
    x = m M.! (1,1)
    y = m M.! (2,1)
    z = m M.! (3,1)

-- | Takes a 'Point3D' and converts it to a 'M.Matrix Double'.
point3DToMatrix
  :: Point3D         -- ^ Point to convert.
  -> M.Matrix Double -- ^ Converted Point
point3DToMatrix (P (x,y,z)) = M.fromList 3 1 [x,y,z]

-- | Creates a 'Fuzzy' set of transformed things ('Transformation').
instance Alg Transformation (Fuzzy Point3D) where
  alg :: Transformation (Fuzzy Point3D) -> Fuzzy Point3D
  alg (Translate t s) = createFuzzy (\p -> characteristicFunction s (matrixToPoint3D((point3DToMatrix p) - t)))
  alg (Rotate r s)    = createFuzzy (\p -> characteristicFunction s (matrixToPoint3D((fromRight (M.fromList 3 3 [1,0,0,0,1,0,0,0,1]) $ M.inverse r) * (point3DToMatrix p))))
  alg (Scale n s)     = createFuzzy (\p -> characteristicFunction s (matrixToPoint3D((M.fromList 3 3 [1/n,0,0,0,1/n,0,0,0,1/n]) * (point3DToMatrix p))))

-- Booleans
-- ----------------

-- | Performs 'Boolean' operations of shapes with the 'Fuzzy' set booleans.
instance Alg Boolean (Fuzzy Point3D) where
  alg :: Boolean (Fuzzy Point3D) -> Fuzzy Point3D
  alg Empty              = emptyFuzzy
  alg Universal          = universalFuzzy
  alg (Union a b)        = unionFuzzy a b
  alg (Intersection a b) = intersectionFuzzy a b
  alg (Complement x)     = complementFuzzy x
  alg (Difference a b)   = differenceFuzzy a b

-- Creating Soft Shapes
-- ----------------------

-- | Softens the membership of elements of the 'Fuzzy' set.
instance Alg Soften (Fuzzy Point3D) where
  alg :: Soften (Fuzzy Point3D) -> Fuzzy Point3D
  alg (ConstSoften n s) = createFuzzy (\p -> n * (characteristicFunction s p))
  alg (FromPointSoften c r s) = createFuzzy charFun
    where 
      charFun :: Point3D -> Double
      charFun p = (1-(d/r)) * (characteristicFunction s p)
        where
          d = euclideanDist (matrixToPoint3D c) p

-- | Creates a 'Fuzzy' set for soft primatives ('PrimSoft').
instance Alg PrimSoft (Fuzzy Point3D) where
  alg :: PrimSoft (Fuzzy Point3D) -> Fuzzy Point3D
  alg (SoftSphere) = createFuzzy softSphere
    where
      softSphere :: Point3D -> Double
      softSphere p = if (d < 1 ) then 1 - d else 0 
        where d = euclideanDist p (P (0,0,0))

-- Modulations
-- ----------------

-- | Modulates the members of the 'Fuzzy' set based their membership ('ModulateDensity').
instance Alg ModulateDensity (Fuzzy Point3D) where
  alg :: ModulateDensity (Fuzzy Point3D) -> Fuzzy Point3D
  alg (ModulateDensity f s) = createFuzzy (f . (characteristicFunction s))

-- | Modulates the members of the 'Fuzzy' set based on where they are ('ModulatePoint').
instance Alg ModulatePoint (Fuzzy Point3D) where
  alg :: ModulatePoint (Fuzzy Point3D) -> Fuzzy Point3D
  alg (ModulatePoint f s) = createFuzzy ((characteristicFunction s) . matrixToPoint3D . f . point3DToMatrix)

-- | Modulates the members of the 'Fuzzy' set based on the whole set ('ModulateGeometry').
instance Alg ModulateGeometry (Fuzzy Point3D) where
  alg :: ModulateGeometry (Fuzzy Point3D) -> Fuzzy Point3D
  alg (ModulateGeometry f s) = createFuzzy ((f ((characteristicFunction s) . matrixToPoint3D)) . point3DToMatrix)