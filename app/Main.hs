module Main where

import Data.Matrix
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Grid
import Neighboring
import Rendering

shouldLive c = (\a -> a >= 2 && a <= 3) . aliveNeighbors c

mapI :: (a -> Int -> c) -> [a] -> [c]
mapI f l = zipWith f l [0 ..]

updateCells :: Grid Cell -> Grid Cell
updateCells g = g
  where
    t = mapI (\a i -> shouldLive $ toc i g) g

main :: IO ()
main = do
  simulate window black fps initialState render update
  where
    update :: ViewPort -> Float -> GameState -> GameState
    update _ _ = GameState . updateCells . cells

render :: GameState -> Picture
render =
  pictures
    . fromCellCoords
    . elementwise (,) cellCoords
    . cells
