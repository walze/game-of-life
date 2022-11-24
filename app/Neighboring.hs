module Neighboring where

import Config
import Game
import Grid
import Prelude hiding (all)

data Neighboring = Neighboring
  { top :: Cell,
    left :: Cell,
    right :: Cell,
    bottom :: Cell,
    all :: [Cell]
  }

neighboring :: Cell -> [Cell] -> Neighboring
neighboring (Vec x y a) g =
  Neighboring t l r b all
  where
    t = get x (y + 1)
    b = get x (y - 1)
    r = get (x + 1) y
    l = get (x - 1) y
    all = [t, l, r, b]
    get x y = g !! fromV (Vec (x `mod` s) (y `mod` s) a) g
    s = round $ size config

aliveNeighbors :: Cell -> [Cell] -> Int
aliveNeighbors c g = foldl (\b (Vec _ _ a) -> b + fromEnum a) 0 $ all $ neighboring c g
