{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

data Vec a = Vec Int Int a deriving (Show, Eq)

sqrtLength :: (Foldable f, Integral b) => f a -> b
sqrtLength = round . sqrt . fromIntegral . length

class (Foldable f, Functor g) => Grid f g where
  get :: Foldable f => f a -> g b -> a
  toInt :: g a -> f b -> Int
  makeGrid :: Int -> f (g ())

instance Functor Vec where
  fmap f (Vec x y a) = Vec x y (f a)

instance Grid [] Vec where
  get fa v = fa !! toInt v fa

  toInt (Vec x y _) fa = sqrtLength fa * x + y

  makeGrid n = [Vec x y () | x <- [1 .. n], y <- [1 .. n]]
