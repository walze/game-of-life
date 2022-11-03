module Game where

import Config
import Graphics.Gloss
import Grid
import Lib

type Cell = Vec Bool

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)
  where
    (Config _ wWidth wHeight _ _) = config

randomCell :: Vec a -> Cell
randomCell (Vec x y _) = Vec x y $ randomBool (x + y)

newtype GameState = GameState
  { cells :: [Cell]
  }

initialState :: GameState
initialState =
  GameState $ randomCell <$> makeGrid s
  where
    s = round $ size config
