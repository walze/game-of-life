{-# LANGUAGE ConstrainedClassMethods #-}

module Grid where

data Vec a = Vec Int Int a deriving (Show, Eq)

sqrtLength :: (Foldable f, Integral b) => f a -> b
sqrtLength = round . sqrt . fromIntegral . length

class Functor f => Grid f where
  get :: Foldable f => f a -> Vec b -> a
  toV :: Int -> f a -> Vec ()
  fromV :: Vec b -> f a -> Int
  makeGrid :: Int -> f (Vec ())

instance Functor Vec where
  fmap f (Vec x y a) = Vec x y (f a)

instance Grid [] where
  get fa v = fa !! fromV v fa

  toV i fa = Vec (i `div` s) (i `mod` s) ()
    where
      s = sqrtLength fa

  fromV (Vec x y _) fa = sqrtLength fa * x + y

  makeGrid n = [Vec x y () | x <- [1 .. n], y <- [1 .. n]]
