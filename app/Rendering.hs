module Rendering where

import Data.Matrix
import Graphics.Gloss

import Game

calc a b c = (fromIntegral a :: Float) - b / 2 + c / 2

fromCellCoords :: Matrix ((Int, Int), Bool) -> Matrix Picture
fromCellCoords = fmap (\((x, y), alive) ->
    translate (calc x wWidth cwSize) (calc y wHeight chSize) $
    color (if alive then white else black) $
    rectangleSolid (max 1 (cwSize - 1)) (max 1 (chSize - 1))
  )
