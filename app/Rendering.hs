module Rendering where

import Game
import Graphics.Gloss
  ( Picture,
    black,
    color,
    rectangleSolid,
    translate,
    white,
  )

fromCellCoords :: [Cell] -> [Picture]
fromCellCoords =
  fmap
    ( \(Cell (x, y) alive) ->
        translate (calc x wWidth cwSize) (calc y wHeight chSize) $
          color (if alive then white else black) $
            rectangleSolid (max 1 (cwSize - 1)) (max 1 (chSize - 1))
    )
  where
    calc :: Integral a => a -> Float -> Float -> Float
    calc a b c = (fromIntegral a :: Float) - b / 2 + c / 2
