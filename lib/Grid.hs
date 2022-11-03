{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

data Vec a = Vec Int Int a deriving (Show, Eq)

sqrtLength :: (Foldable f, Integral b) => f a -> b
sqrtLength = round . sqrt . fromIntegral . length

class Functor f => Grid f v where
  get :: Foldable f => f a -> v b -> a
  toV :: Int -> f a -> v ()
  fromV :: v b -> f a -> Int
  makeGrid :: Int -> f (v ())
  (<<$>>) :: (a -> b) -> f (v a) -> f (v b)

instance Functor Vec where
  fmap f (Vec x y a) = Vec x y (f a)

instance Grid [] Vec where
  (<<$>>) = fmap . fmap

  get fa v = fa !! fromV v fa

  toV i fa = Vec (i `div` s) (i `mod` s) ()
    where
      s = sqrtLength fa

  fromV (Vec x y _) fa = sqrtLength fa * x + y

  makeGrid n = [Vec x y () | x <- [1 .. n], y <- [1 .. n]]
