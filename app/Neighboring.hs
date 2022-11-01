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
    t = g !! fromc (x, y + 1) g
    l = g !! fromc (x - 1, y) g
    r = g !! fromc (x + 1, y) g
    b = g !! fromc (x, y - 1) g
    all = [t, l, r, b]

aliveNeighbors :: Coords Int -> Grid Cell -> Int
aliveNeighbors c g = foldl (\b a -> b + fromEnum a) 0 $ all $ neighboring c g
