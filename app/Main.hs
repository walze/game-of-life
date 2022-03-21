module Main(main) where

import Rendering
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Neighboring

import Data.Matrix

shouldLive :: (Int, Int) -> Matrix Cell -> Bool
shouldLive (x, y) = maybe False (\a -> a >= 2 && a <= 3)
  . aliveNeighbors
  . neighboring (x, y)

updateCells :: Matrix Cell -> Matrix Cell
updateCells grid = mapPos (\(x, y) alive -> shouldLive (x, y) grid) grid

main :: IO ()
main = do
  simulate window black fps initialState render update

  where
    update :: ViewPort -> Float -> GameState -> GameState
    update _ _ = GameState . updateCells . cells

    render :: GameState -> Picture
    render = pictures
           . toList
           . fromCellCoords
           . elementwise (,) cellCoords
           . cells
