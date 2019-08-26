{- |
Module      :  HypertextureLanguage.MayaInstance
Maintainer  :  sf16540@my.bristol.ac.uk

Instance of the 'Hypertexture' language that outputs to the 3D modelling
environment Maya using its internal langauge MEL. This instance will take
advantage of Maya's primative shapes and boolean functions.

-}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TypeOperators         #-}

module MayaInstance (
  MEL(createCode),
  MELLang,
  PrismMEL,
  getEulerAngles,
  BooleanMEL,

  circleMEL,
  triangleMEL,
  squareMEL,

  cubeMEL,
  cylinderMEL,
  sphereMEL,
  coneMEL,
  torusMEL,

  prism3DMEL,

  translateMEL,
  rotateMEL,
  scaleMEL,

  unionMEL,
  intersectionMEL,
  differenceMEL,

) where

import DeepEmbedding
import DeepEmbedding.Infrastructure
import DeepEmbedding.Shapes2D
import DeepEmbedding.Shapes3D
import DeepEmbedding.Transformation
import DeepEmbedding.Boolean
import DeepEmbedding.CreatingSoftShapes
import DeepEmbedding.Modulate

import Data.Fix
import qualified "matrix" Data.Matrix as M

data MEL = MEL {
  -- | Function that will produce code for an object given its name.
  createCode :: (String -> String)
}

instance Show MEL where
  show (MEL f) = f "name"

--                         Maya MEL Language
-- ===================================================================

type MELLang = Fix (Prim2D :+: Prim3D :+: PrismMEL :+: Transformation :+: BooleanMEL)

circleMEL :: MELLang
circleMEL = Fix (L(L(L(L(Circle)))))
triangleMEL :: MELLang
triangleMEL = Fix (L(L(L(L(Triangle)))))
squareMEL :: MELLang
squareMEL = Fix (L(L(L(L(Square)))))

cubeMEL :: MELLang
cubeMEL = Fix (L(L(L(R(Cube)))))
cylinderMEL :: MELLang
cylinderMEL = Fix (L(L(L(R(Cylinder)))))
sphereMEL :: MELLang
sphereMEL = Fix (L(L(L(R(Sphere)))))
coneMEL :: MELLang
coneMEL = Fix (L(L(L(R(Cone)))))
torusMEL :: MELLang
torusMEL = Fix (L(L(L(R(Torus)))))

prism3DMEL :: Int -> MELLang
prism3DMEL n = Fix (L(L(R(PrismMEL n))))

translateMEL :: (M.Matrix Double) -> MELLang -> MELLang
translateMEL t x = Fix (L(R(Translate t x)))
rotateMEL :: (M.Matrix Double) -> MELLang -> MELLang
rotateMEL r x = Fix (L(R(Rotate r x)))
scaleMEL :: Double -> MELLang -> MELLang
scaleMEL s x = Fix (L(R(Scale s x)))

unionMEL :: MELLang -> MELLang -> MELLang
unionMEL a b = Fix (R(UnionMEL a b))
intersectionMEL :: MELLang -> MELLang -> MELLang
intersectionMEL a b = Fix (R(IntersectionMEL a b))
differenceMEL :: MELLang -> MELLang -> MELLang
differenceMEL a b = Fix (R(DifferenceMEL a b))


--                         Maya MEL Instance
-- ===================================================================

-- | Converts 2D primatives ('Prim2D') to MEL code, taking advantage of primitives
--   where possible.
instance Alg Prim2D MEL where
  alg :: Prim2D MEL -> MEL
  alg Circle      = MEL (\name->"\npolyDisc -sides 3 -subdivisionMode 4 -subdivisions 4 -radius 1;\nsetAttr \"pDisc1.rotateX\" 90;\n")-- NOTE:- so hope Maya's default remains pDisc1
  alg Triangle    = MEL (\name->"\npolyPlane -w 1 -h 1 -sx 1 -sy 1 -ax 0 0 1 -cuv 2 -ch 1 -name \""++ name ++"\";\nsetAttr \""++ name ++".translateX\" 0.5;\nsetAttr \""++ name ++".translateY\" 0.5;\nselect -r "++ name ++".vtx[3] ;\ndoDelete;\n")
  alg Square      = MEL (\name->"\npolyPlane -w 2 -h 2 -sx 1 -sy 1 -ax 0 0 1 -cuv 2 -ch 1 -name \""++ name ++"\";\n")

-- | Converts 3D primatives ('Prim3D') to MEL code, taking advantage of primitives
--   where possible.
instance Alg Prim3D MEL where
  alg :: Prim3D MEL -> MEL
  alg Cube     = MEL (\name->"\npolyCube -w 2 -h 2 -d 2 -sx 1 -sy 1 -sz 1 -ax 0 1 0 -cuv 4 -ch 1 -name \""++ name ++"\";\n")
  alg Cylinder = MEL (\name->"\npolyCylinder -r 1 -h 1 -sx 20 -sy 1 -sz 1 -ax 0 0 1 -rcp 0 -cuv 3 -ch 1 -name \""++ name ++"\";\nsetAttr \""++ name ++".translateZ\" 0.5;\n")
  alg Sphere   = MEL (\name->"\npolySphere -r 1 -sx 20 -sy 20 -ax 0 0 1 -cuv 2 -ch 1 -name \""++ name ++"\";\n")
  alg Cone     = MEL (\name->"\npolyCone -r 1 -h 1 -sx 20 -sy 1 -sz 0 -ax 0 0 1 -rcp 0 -cuv 3 -ch 1 -name \""++ name ++"\";\nsetAttr \""++ name ++".translateZ\" 0.5;\n")
  alg Torus    = MEL (\name->"\npolyTorus -r 1 -sr 0.5 -tw 0 -sx 20 -sy 20 -ax 0 0 1 -cuv 1 -ch 1 -name \""++ name ++"\";\n")

-- | Maya has a prism primative so it is better to introduce a new part
--   of the coproduct.
data PrismMEL k
  -- | A Maya Prism with n sides and a height of 1.
  = PrismMEL Int
  deriving (Show, Functor)

-- | Converts 'PrismMEL' to MEL code.
instance Alg PrismMEL MEL where
  alg :: PrismMEL MEL -> MEL
  alg (PrismMEL n) = MEL (\name->("\npolyPrism -l 1 -w 1 -ns " ++ (show n) ++" -sh 1 -sc 0 -ax 0 0 1 -cuv 3 -ch 1 -name \""++ name ++"\";\nsetAttr \""++ name ++".translateZ\" 0.5;\n")) -- NOTE:- again gets bigger with number of sides.

-- | Produces MEL 'Transformation' code.
instance Alg Transformation MEL where
  alg (Translate t s) = MEL (\name->(concat [(createCode s name)
                                    , "select -r ", name,";\n"
                                    , "move -r ", (show (t M.! (1,1)))," ", (show (t M.! (2,1)))," ", (show (t M.! (3,1))),";\n"]))
  alg (Rotate r s)    = MEL (\name->(concat [(createCode s name)
                                    , "select -r ", name,";\n"
                                    , "rotate -r -os -fo ",(show x)," ",(show y)," ",(show z)," ;\n"]))
                        where
                          (x,y,z) = getEulerAngles r
  alg (Scale n s)     = MEL (\name->(concat [(createCode s name)
                              ,"select -r ", name,";\n"
                              ,"scale -r ", (show n)," ", (show n)," ", (show n),";\n"]))

-- | Extracts euler angles from rotation matrix.
getEulerAngles
  :: M.Matrix Double -- ^ Should be a 3x3 rotation matrix.
  -> (Double,Double,Double)
getEulerAngles m = (90*x,90*y,90*z)
  where
    x = atan2 (m M.! (3,2)) (m M.! (3,3))
    y = atan2 (-(m M.! (3,1))) (sqrt((m M.! (3,2))^2+(m M.! (3,3))^2))
    z = atan2 (m M.! (2,1)) (m M.! (1,1))

-- | Maya offers less boolean operations than the 'DeepEmbedding'.
data BooleanMEL k
  = UnionMEL k k
  | IntersectionMEL k k
  | DifferenceMEL k k
  deriving (Show, Functor)

-- | Produces MEL 'Boolean' code using Maya booleans.
instance Alg BooleanMEL MEL where
  alg (UnionMEL s1 s2) = MEL (\name->(concat [(createCode s1 (name ++"a")), (createCode s2 (name ++"b"))
                        ,"select -r ",(name ++"a"),";\n"
                        ,"select -r ",(name ++"b"),";\n"
                        ,"polyCBoolOp -op 1 -ch 0 -preserveColor 0 -classification 1 -name \"",name,"\" ",(name ++"a")," ",(name ++"a")," ",(name ++"b"),";\n"]))
  alg (IntersectionMEL s1 s2) = MEL (\name->(concat [(createCode s1 (name ++"a")), (createCode s2 (name ++"b"))
                        ,"select -r ",(name ++"a"),";\n"
                        ,"select -r ",(name ++"b"),";\n"
                        ,"polyCBoolOp -op 3 -ch 0 -preserveColor 0 -classification 1 -name \"",name,"\" ",(name ++"a")," ",(name ++"a")," ",(name ++"b"),";\n"]))
  alg (DifferenceMEL s1 s2) = MEL (\name->(concat [(createCode s1 (name ++"a")), (createCode s2 (name ++"b"))
                        ,"select -r ",(name ++"a"),";\n"
                        ,"select -r ",(name ++"b"),";\n"
                        ,"polyCBoolOp -op 2 -ch 0 -preserveColor 0 -classification 1 -name \"",name,"\" ",(name ++"a")," ",(name ++"a")," ",(name ++"b"),";\n"]))