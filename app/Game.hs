module Game where

import Config
import Graphics.Gloss
import Grid
import Lib

data Cell = Cell (Int, Int) Bool

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)
  where
    (Config _ wWidth wHeight _ _) = config

randomCell :: Coords Int -> Cell
randomCell c@(x, y) = Cell c $ randomBool (x + y)

newtype GameState = GameState
  { cells :: Grid Cell
  }

initialState :: GameState
initialState =
  GameState $ randomCell <$> makeGrid s
  where
    s = round $ size config
