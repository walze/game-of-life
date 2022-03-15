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
