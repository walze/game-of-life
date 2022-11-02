module Game where

import Graphics.Gloss
import Grid
import System.Random

data Cell = Cell (Int, Int) Bool

fps :: Int
fps = 50

wWidth :: Float
wWidth = 1200.0

wHeight :: Float
wHeight = 800.0

gSize :: Float
gSize = 50.0

cwSize :: Float
cwSize = wWidth / gSize

chSize :: Float
chSize = wHeight / gSize

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)

rb :: Int -> Bool
rb i = head $ randoms (mkStdGen i)

grid = (\c@(x, y) -> Cell c $ rb (x + y)) <$> makeGrid (round gSize)

newtype GameState = GameState
  { cells :: Grid Cell
  }

initialState :: GameState
initialState =
  GameState grid
