module Rendering where

import Config
import Game (Cell (..))
import Graphics.Gloss
  ( Picture,
    black,
    color,
    rectangleSolid,
    translate,
    white,
  )
import Grid
import Lib

calc :: Int -> Float -> Float -> Float
calc p' w c = -w / 2 + p * c + c / 2
  where
    p = fromIntegral p'

cellPictures :: [Cell] -> [Picture]
cellPictures =
  fmap
    ( \(Vec x y alive) ->
        translate (calc x w cw) (calc y h ch) $
          color (alive ? white :? black) $
            rectangleSolid cw ch
    )
  where
    (Config _ w h cw ch) = config
