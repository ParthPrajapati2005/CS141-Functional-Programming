module Currying where

curry' :: ((a, b) -> t) -> a -> b -> t
curry' f = \x -> \y -> f(x,y)

uncurry' :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry' f = \(x,y) -> f x y