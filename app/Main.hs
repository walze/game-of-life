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

updateCell :: Int -> [Cell] -> Cell
updateCell i g = Vec x y $ shouldLive c g
  where
    c@(Vec x y a) = toV i g

updateCells :: [Cell] -> [Cell]
updateCells g = mapI f g
  where
    f _ i = updateCell i g

render :: GameState -> Picture
render =
  pictures
    . cellPictures
    . cells

update :: ViewPort -> Float -> GameState -> GameState
update _ _ = GameState . updateCells . cells

main :: IO ()
main = simulate window black 15 initialState render update
