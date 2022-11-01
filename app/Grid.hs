module Grid where

import Data.Functor.Identity (Identity)
import System.Random

type Coords a = (a, a)

type Grid a = [a]

fromc (x, y) s = y * s + x

toc i s = (i `div` s, i `mod` s)

randomBool i = randoms (mkStdGen i) :: [Bool]

makeGrid :: Int -> [Bool]
makeGrid n = [head (randomBool i) | i <- [1 .. n * n]]
