module Game where

import Data.Matrix
import Graphics.Gloss
import Data.Bifunctor
import Control.Monad (foldM)

fps :: Int
fps = 15

wWidth :: Float
wWidth = 1200.0
wHeight :: Float
wHeight = 800.0
gSize :: Float
gSize = 5.0

cwSize :: Float
cwSize = wWidth / gSize
chSize :: Float
chSize = wHeight / gSize

window :: Display
window = InWindow "Game of Life" (round wWidth, round wHeight) (10, 10)

grid :: Matrix (Float, Float)
grid = bimap fromIntegral fromIntegral <$> matrix (round gSize) (round gSize) id

cellCoords :: Matrix (Int, Int)
cellCoords = bimap (round . (cwSize *) . (+ (-1))) (round . (chSize *) . (+ (-1))) <$> grid


type Cell = ((Int, Int), Bool)

newtype GameState = GameState {
  cells :: Matrix Cell
} deriving (Show)

initialState :: GameState
initialState = GameState
  $ fmap (\(a, b) -> ((a, b), b `mod` 3 == 0)) cellCoords

data Neighboring a = Neighboring {
  top :: Maybe a,
  left :: Maybe a,
  right :: Maybe a,
  bottom :: Maybe a,
  topleft :: Maybe a,
  topright :: Maybe a,
  bottomleft :: Maybe a,
  bottomright :: Maybe a
} deriving (Show)

neighboring :: (Int, Int) -> Matrix a -> Neighboring a
neighboring (x, y) g = Neighboring {
    top = t,
    left = l,
    right = r,
    bottom = b,
    topleft = tl,
    topright = tr,
    bottomleft = bl,
    bottomright = br
  }
  where
    t  = safeGet   x      (y + 1) g
    l  = safeGet  (x - 1)  y      g
    r  = safeGet  (x + 1)  y      g
    b  = safeGet   x      (y - 1) g
    tl = safeGet  (x - 1) (y + 1) g
    tr = safeGet  (x + 1) (y + 1) g
    bl = safeGet  (x - 1) (y - 1) g
    br = safeGet  (x + 1) (y - 1) g

aliveNeighbors :: Neighboring Cell -> Maybe Int
aliveNeighbors a = foldM (\b cell ->
    case cell of
      Nothing -> Just b
      Just (_, alive) -> Just (b + fromEnum alive)
    ) 0
  $ neighbors
  <*> [a]
  where
    neighbors = [top, left, right, bottom, topleft, topright, bottomleft, bottomright]
