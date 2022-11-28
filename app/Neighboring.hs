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
neighboring (Vec x y a) g = Neighboring t l r b all
  where
    s = round $ size config
    get x y = Grid.get g (Vec (x `mod` s) (y `mod` s) a)
    t = get x (y + 1)
    b = get x (y - 1)
    r = get (x + 1) y
    l = get (x - 1) y
    all = [t, l, r, b]

aliveNeighbors :: Cell -> [Cell] -> Int
aliveNeighbors c = foldl (\b (Vec _ _ a) -> b + fromEnum a) 0 . all . neighboring c
