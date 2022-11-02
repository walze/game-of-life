module Main where

import Config
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Grid
import Lib
import Neighboring
import Rendering

isAlive n = n >= 2 && n <= 3

shouldLive c = isAlive . aliveNeighbors c

updateCells :: Grid Cell -> Grid Cell
updateCells g = mapI f g
  where
    f (Cell c a) i = Cell c $ shouldLive (toc i s) g
    s = round $ size config

main :: IO ()
main = do
  simulate window black 15 initialState render update
  where
    render :: GameState -> Picture
    render =
      pictures
        . cellPictures
        . cells

    update :: ViewPort -> Float -> GameState -> GameState
    update _ _ = GameState . updateCells . cells
