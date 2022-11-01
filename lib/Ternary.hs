module Ternary where

infixl 0 ?

infixl 1 :?

data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y
