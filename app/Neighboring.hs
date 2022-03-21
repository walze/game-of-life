module Neighboring where
import Data.Matrix (Matrix, safeGet)
import Game (Cell)
import Control.Monad (foldM)

data Neighboring a = Neighboring {
  top :: Maybe a,
  left :: Maybe a,
  right :: Maybe a,
  bottom :: Maybe a,
  topleft :: Maybe a,
  topright :: Maybe a,
  bottomleft :: Maybe a,
  bottomright :: Maybe a
} deriving (Show)

neighboring :: (Int, Int) -> Matrix a -> Neighboring a
neighboring (x, y) g = Neighboring {
    top = t,
    left = l,
    right = r,
    bottom = b,
    topleft = tl,
    topright = tr,
    bottomleft = bl,
    bottomright = br
  }
  where
    t  = safeGet   x      (y + 1) g
    l  = safeGet  (x - 1)  y      g
    r  = safeGet  (x + 1)  y      g
    b  = safeGet   x      (y - 1) g
    tl = safeGet  (x - 1) (y + 1) g
    tr = safeGet  (x + 1) (y + 1) g
    bl = safeGet  (x - 1) (y - 1) g
    br = safeGet  (x + 1) (y - 1) g

aliveNeighbors :: Neighboring Cell -> Maybe Int
aliveNeighbors a =
  foldM (\b cell ->
    case cell of
      Nothing -> Just b
      Just alive -> Just (b + fromEnum alive)
    ) 0
  $ neighbors
  <*> [a]
  where
    neighbors = [top, left, right, bottom, topleft, topright, bottomleft, bottomright]

