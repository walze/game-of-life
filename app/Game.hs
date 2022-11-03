module Game where

import Config
import Graphics.Gloss
import Lib

data Cell = Vec Bool

type Grid = [Cell]

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)
  where
    (Config _ wWidth wHeight _ _) = config

randomCell :: Cell -> Cell
randomCell c@(Vec x y _) = Cell c $ randomBool (x + y)

newtype GameState = GameState
  { cells :: Grid
  }

initialState :: GameState
initialState =
  GameState $ randomCell <$> makeGrid s
  where
    s = round $ size config
