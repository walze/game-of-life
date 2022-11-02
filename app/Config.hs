module Config where

data Config = Config
  { size :: Float,
    width :: Float,
    height :: Float,
    cellWidth :: Float,
    cellHeight :: Float
  }

w = 1200.0

h = 800.0

s = 50.0

cw = w / s

ch = w / s

config :: Config
config = Config s w h cw ch
