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

offset :: Int -> Float -> Float -> Float
offset position size container = -size / 2 + fromIntegral position * container + container / 2

rect :: Config -> Cell -> Picture
rect (Config _ w h cw ch) (Vec x y alive) =
  translate (offset x w cw) (offset y h ch) $
    color (alive ? white :? black) $
      rectangleSolid cw ch

cellPictures :: [Cell] -> [Picture]
cellPictures = (rect config <$>)
