module Game where

import Graphics.Gloss
import Grid

type Cell = Bool

fps :: Int
fps = 5

wWidth :: Float
wWidth = 1200.0

wHeight :: Float
wHeight = 800.0

gSize :: Float
gSize = 10.0

cwSize :: Float
cwSize = wWidth / gSize

chSize :: Float
chSize = wHeight / gSize

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)

grid :: Grid Bool
grid = makeGrid (round gSize)

newtype GameState = GameState
  { cells :: Grid Cell
  }

initialState :: GameState
initialState =
  GameState grid
