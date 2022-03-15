module Main(main) where

import Rendering
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Matrix (toList)

a = neighboring (1, 1) (cells initialState)

main :: IO ()
main = do
  print a
  print $ aliveNeighbors a

  simulate window black fps initialState render update
  where
    update :: ViewPort -> Float -> GameState -> GameState
    update _ _ state = state

    render :: GameState -> Picture
    render state = pictures $ toList $ fromCellCoords (cells state)
