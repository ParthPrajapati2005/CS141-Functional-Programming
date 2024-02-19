module Functors where

data MyList a = Nil | Cons a (MyList a) deriving (Show, Eq)

--After recalling the map function we realise that it only works for lists. We want to make it work for other data types as well
--Lets use Functors

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

instance Foldable MyList where
  foldr f z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Functor MyList where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
