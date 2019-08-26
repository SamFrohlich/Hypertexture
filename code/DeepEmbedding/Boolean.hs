{- |
Module      :  HypertextureLanguage.DeepEmbedding.Boolean
Maintainer  :  sf16540@my.bristol.ac.uk

Language for descibing boolean operations on 2D and 3D shapes.
Has the semantic model of sets of 2D or 3D points.

-}

{-# LANGUAGE DeriveFunctor #-}

module DeepEmbedding.Boolean (
  Boolean(Empty, Universal, Union, Intersection, Complement, Difference)
) where

-- | The implementation should fufil the following set theory properties. 
--   If it is implemented as the denotational semantics suggest these 
--   should come for free.
--
-- Commutative Laws:
--
-- prop> Union a b        = Union b a
-- prop> Intersection a b = Intersection b a
--
-- Associative laws:
--
-- prop> Union (Union a b) c               = Union a (Union b c)
-- prop> Intersection (Intersection a b) c = Intersection a (Intersection b c)
--
-- Distributive laws:
--
-- prop> Union a (Intersection b c) = Intersection (Union a b) (Union a c)
-- prop> Intersection a (Union b c) = Union (Intersection a b) (Intersection a c)
--
-- Identity laws:
--
-- prop> Union a Empty            = a
-- prop> Intersection a Universal = a
--
-- Idempotent laws:
--
-- prop> Union a a     = a
-- prop> Intersect a a = a
--
-- Domination laws:
--
-- prop> Union a Universal    = Universal
-- prop> Intersection a Empty = Empty
--
-- Absorption laws:
--
-- prop> Union a (Intersection a b) = a
-- prop> Intersection a (Union a b) = a
--
-- De Morgan's laws:
--
-- prop> Complement (Union a b)        = Intersection (Complement a) (Complement b)
-- prop> Complement (Intersection a b) = Union (Complement a) (Complement b)
--
-- Involution law:
--
-- prop> Complement (Complement a) = a
--
-- Complement laws:
--
-- prop> Complement Empty     = Universal
-- prop> Complement Universal = Empty
--
data Boolean k 
  -- | Empty set.
  = Empty
  -- | Universal set.
  | Universal
  -- | Set union.
  | Union k k
  -- | Set intersection.
  | Intersection k k
  -- | Set complement.
  | Complement k
  -- | Set differnce.
  | Difference k k
  deriving (Show, Functor)