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

calc :: Int -> Float -> Float -> Float
calc p' w c = -w / 2 + p * c + c / 2
  where
    p = fromIntegral p'

fromCellCoords :: [Cell] -> [Picture]
fromCellCoords =
  fmap
    ( \(Cell (x, y) alive) ->
        translate (calc x wWidth cwSize) (calc y wHeight chSize) $
          color (if alive then white else black) $
            rectangleSolid cwSize chSize
    )
