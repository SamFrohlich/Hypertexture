{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module DeepEmbedding.Infrastructure (
  (:+:)(L,R),
  (\/),
  Alg(alg)
  ) where

data (:+:) f g k  = L (f k)
                  | R (g k)
                  deriving Show

instance (Functor f, Functor g) => Functor (f :+: g) where 
  fmap f (L x) = L (fmap f x)
  fmap f (R y) = R (fmap f y)

(\/) :: (f a -> a) -> (g a -> a) -> ((f :+: g) a -> a)
(algF \/ algG) (L x) = algF x
(algF \/ algG) (R x) = algG x

class Functor f => Alg f a where
  alg :: f a -> a

instance (Alg f a , Alg g a) => Alg (f :+: g) a where
  alg (L x) = alg x
  alg (R x) = alg x