module Grid where

import Data.Functor.Identity (Identity)
import System.Random

type Coords a = (a, a)

type Grid a = [a]

fromc (x, y) s = y * s + x

toc i s = (i `div` s, i `mod` s)

makeGrid :: Int -> [Coords Int]
makeGrid n = [(x, y) | x <- [1 .. n], y <- [1 .. n]]
