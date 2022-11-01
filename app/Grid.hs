module Grid where

import Data.Functor.Identity (Identity)

type Coords a = (a, a)

type Grid a = [a]

toc i g = (i `mod` length g, i `div` length g)

fromc (x, y) g = x + y * length g

makeGrid n = [True | _ <- [1 .. n * n]]
