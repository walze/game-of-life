module Neighboring where

import Game
import Grid
import Prelude hiding (all)

data Neighboring a = Neighboring
  { top :: Cell,
    left :: Cell,
    right :: Cell,
    bottom :: Cell,
    all :: [Cell]
  }

neighboring :: Coords Int -> Grid Cell -> Neighboring a
neighboring (x, y) g =
  Neighboring t l r b all
  where
    t = get x (y + 1)
    b = get x (y - 1)
    r = get (x + 1) y
    l = get (x - 1) y
    all = [t, l, r, b]
    get x y = g !! fromc (x `mod` round gSize, y `mod` round gSize) s
    s = round gSize

aliveNeighbors :: Coords Int -> Grid Cell -> Int
aliveNeighbors c g = foldl (\b a -> b + fromEnum a) 0 $ all $ neighboring c g
