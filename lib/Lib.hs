module Lib where

import System.Random

infixl 0 ?

infixl 1 :?

data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

mapI :: (a -> Int -> c) -> [a] -> [c]
mapI f l = zipWith f l [0 ..]

randomBool :: Int -> Bool
randomBool i = head $ randoms (mkStdGen i)
