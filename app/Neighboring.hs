module Neighboring where

import Config
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
    get x y = g !! fromc (x `mod` s, y `mod` s) s
    s = round $ size config

aliveNeighbors :: Coords Int -> Grid Cell -> Int
aliveNeighbors c g = foldl (\b (Cell _ a) -> b + fromEnum a) 0 $ all $ neighboring c g
