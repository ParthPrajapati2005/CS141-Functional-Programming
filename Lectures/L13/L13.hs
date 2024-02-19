module L13 where

--Recursive declaration of a Mylist
data MyList a = Nil | Cons a (MyList a) deriving (Show, Eq)

oneTwoThree :: MyList Int
oneTwoThree = Cons 1 (Cons 2 (Cons 3 Nil))

--Checks if a myList starts with 5
startsWithFive :: MyList Int -> Bool
startsWithFive (Cons 5 _) = True
startsWithFive _ = False

--Converts a regular list to a MyList
toMyList :: [a] -> MyList a
toMyList = foldr Cons Nil

foldrOnNewList :: (a -> b -> b) -> b -> MyList a -> b
foldrOnNewList f z Nil = z
foldrOnNewList f z (Cons x xs) = f x (foldrOnNewList f z xs)

-- This is the other direction
-- Running foldrOnNewList (:) [] (Cons 1 (Cons 2 (Cons 3 Nil))) will give [1,2,3]

-----------------------------------------------------------------------------------------------------
--FOLDABLE TYPE CLASS

--Class declaration of Foldable
class Foldable' t where
  foldr' :: (a -> b -> b) -> b -> t a -> b

--Instance declaration of Foldable for MyList
instance Foldable' MyList where
    foldr' f z Nil = z
    foldr' f z (Cons x xs) = f x (foldr' f z xs)

--We can now make 2 functions which can convert between normal lists and MyLists

fromMyList :: MyList a -> [a]
fromMyList xs = foldr' (:) [] xs  --Here we are useing the special foldr which we declared above

toMyList' :: [a] -> MyList a
toMyList' xs = foldr Cons Nil xs  --Here we are using the normal foldr function
