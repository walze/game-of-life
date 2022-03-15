module Main(main) where

import Rendering
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Neighboring

import Data.Matrix

shouldLive (x, y) grid = maybe False (\a -> a >= 2 && a <= 3) count
  where
    count = aliveNeighbors $ neighboring (x, y) grid

asdf :: Matrix Cell -> Matrix Cell
asdf grid = mapPos (\(x, y) (p, alive) -> (p, shouldLive (x, y) grid)) grid

main :: IO ()
main = do

  simulate window black fps initialState render update
  where
    update :: ViewPort -> Float -> GameState -> GameState
    update _ _ state = GameState $ asdf $ cells state

    render :: GameState -> Picture
    render state = pictures $ toList $ fromCellCoords (cells state)
