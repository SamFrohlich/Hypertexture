{- |
Module      :  HypertextureLanguage.DeepEmbedding
Maintainer  :  sf16540@my.bristol.ac.uk

Deep embedding of a language for describing 'Hypertexture'.
-}

{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}

module DeepEmbedding (
  Alg(alg),
  -- * Full Co-Product
  Hypertexture,
  circle,
  triangle,
  square,

  implicit2D,

  cube,
  cylinder,
  sphere,
  cone,
  torus,

  implicit3D,

  prism3D,

  translate,
  rotate,
  scale,

  empty,
  universal,
  union,
  intersection,
  complement,
  difference,

  constSoften,
  fromPointSoften,

  softSphere,

  modulateDensity,

  modulatePoint,

  modulateGeometry,
  -- * 2D Language
  -- | An example of a language for describing 2D primative shapes using a smaller coproduct.
  Lang2D,
  circle2D,
  triangle2D,
  square2D,

  translate2D,
  rotate2D,
  scale2D,

  empty2D,
  universal2D,
  union2D,
  intersection2D,
  complement2D,
  difference2D,

  ) where

import DeepEmbedding.Infrastructure
import DeepEmbedding.Shapes2D
import DeepEmbedding.Shapes3D
import DeepEmbedding.Transformation
import DeepEmbedding.Boolean
import DeepEmbedding.CreatingSoftShapes
import DeepEmbedding.Modulate
import Point
import qualified "matrix" Data.Matrix as M
import Data.Fix

-- | Full coproduct of language.
--   Users of the langauge can pick and choose what parts they want and
--   how to restrict what parts go where. 
type Hypertexture = Fix (Prim2D :+: Implicit2D :+: Prim3D :+: Implicit3D :+: Prism3D :+: Transformation :+: Boolean :+: Soften :+: PrimSoft :+: ModulateDensity :+: ModulatePoint :+: ModulateGeometry)

--                         Full Co-Product
-- ===================================================================

circle :: Hypertexture
circle = Fix (L(L(L(L(L(L(L(L(L(L(L(Circle))))))))))))
triangle :: Hypertexture
triangle = Fix (L(L(L(L(L(L(L(L(L(L(L(Triangle))))))))))))
square :: Hypertexture
square = Fix (L(L(L(L(L(L(L(L(L(L(L(Square))))))))))))

implicit2D :: (Double -> Double -> Double) -> Hypertexture
implicit2D f =  Fix (L(L(L(L(L(L(L(L(L(L(R(Implicit2D f))))))))))))

cube :: Hypertexture
cube = Fix (L(L(L(L(L(L(L(L(L(R(Cube)))))))))))
cylinder :: Hypertexture
cylinder = Fix (L(L(L(L(L(L(L(L(L(R(Cylinder)))))))))))
sphere :: Hypertexture
sphere = Fix (L(L(L(L(L(L(L(L(L(R(Sphere)))))))))))
cone :: Hypertexture
cone = Fix (L(L(L(L(L(L(L(L(L(R(Cone)))))))))))
torus :: Hypertexture
torus = Fix (L(L(L(L(L(L(L(L(L(R(Torus)))))))))))

implicit3D :: (Double -> Double -> Double -> Double) -> Hypertexture
implicit3D f = Fix (L(L(L(L(L(L(L(L(R(Implicit3D f))))))))))

prism3D :: Hypertexture -> Hypertexture
prism3D x = Fix (L(L(L(L(L(L(L(R(Prism3D x)))))))))

translate :: (M.Matrix Double) -> Hypertexture -> Hypertexture
translate t x = Fix (L(L(L(L(L(L(R(Translate t x))))))))
rotate :: (M.Matrix Double) -> Hypertexture -> Hypertexture
rotate r x = Fix (L(L(L(L(L(L(R(Rotate r x))))))))
scale :: Double -> Hypertexture -> Hypertexture
scale s x = Fix (L(L(L(L(L(L(R(Scale s x))))))))

empty :: Hypertexture
empty = Fix (L(L(L(L(L(R(Empty)))))))
universal :: Hypertexture
universal = Fix (L(L(L(L(L(R(Universal)))))))
union :: Hypertexture -> Hypertexture -> Hypertexture
union a b = Fix (L(L(L(L(L(R(Union a b)))))))
intersection :: Hypertexture -> Hypertexture -> Hypertexture
intersection a b = Fix (L(L(L(L(L(R(Intersection a b)))))))
complement :: Hypertexture -> Hypertexture
complement x = Fix (L(L(L(L(L(R(Complement x)))))))
difference :: Hypertexture -> Hypertexture -> Hypertexture
difference a b = Fix (L(L(L(L(L(R(Difference a b)))))))

constSoften :: Double -> Hypertexture -> Hypertexture
constSoften s x = Fix (L(L(L(L(R(ConstSoften s x))))))
fromPointSoften :: (M.Matrix Double) -> Double -> Hypertexture -> Hypertexture
fromPointSoften p s x = Fix (L(L(L(L(R(FromPointSoften p s x))))))

softSphere :: Hypertexture
softSphere = Fix (L(L(L(R(SoftSphere)))))

modulateDensity :: (Double -> Double) -> Hypertexture -> Hypertexture
modulateDensity f x = Fix (L(L(R(ModulateDensity f x))))

modulatePoint :: (Point -> Point) -> Hypertexture -> Hypertexture
modulatePoint f x = Fix (L(R(ModulatePoint f x)))

modulateGeometry :: ((Point -> Double)->(Point -> Double))  -> Hypertexture -> Hypertexture
modulateGeometry f x = Fix (R(ModulateGeometry f x))

--                     Just 2D
-- ===================================================================

-- An example of a language for describing 2D primative shapes using a smaller coproduct.

type Lang2D = Fix (Prim2D :+: Transformation :+: Boolean)

circle2D :: Lang2D
circle2D = Fix (L(L(Circle)))
triangle2D :: Lang2D
triangle2D = Fix (L(L(Triangle)))
square2D :: Lang2D
square2D= Fix (L(L(Square)))

translate2D :: (M.Matrix Double) -> Lang2D -> Lang2D
translate2D t x = Fix (L(R(Translate t x)))
rotate2D :: (M.Matrix Double) -> Lang2D -> Lang2D
rotate2D r x = Fix (L(R(Rotate r x)))
scale2D :: Double -> Lang2D -> Lang2D
scale2D s x = Fix (L(R(Scale s x)))

empty2D :: Lang2D
empty2D = Fix (R(Empty))
universal2D :: Lang2D
universal2D = Fix (R(Universal))
union2D :: Lang2D -> Lang2D -> Lang2D
union2D a b = Fix (R(Union a b))
intersection2D :: Lang2D -> Lang2D -> Lang2D
intersection2D a b = Fix (R(Intersection a b))
complement2D :: Lang2D -> Lang2D
complement2D x = Fix (R(Complement x))
difference2D :: Lang2D -> Lang2D -> Lang2D
difference2D a b = Fix (R(Difference a b))