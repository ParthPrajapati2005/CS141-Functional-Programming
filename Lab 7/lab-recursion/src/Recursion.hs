module Recursion where
import Prelude hiding (or, zip, zipWith)

--------------------------------------------------------------------------------

-- | Takes a list of boolean values and returns True if any of them is True.
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs
 
-- | or implemented using implicit recursion (the `any` function) - The any function takes in 2 arguments - a function with return type of bool and 
or' :: [Bool] -> Bool
or' lst = any (== True) lst


--------------------------------------------------------------------------------

-- | Takes a list of Maybe values and collects together all the Just ones.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (x:xs) = case x of 
                   Nothing -> catMaybes xs
                   Just a -> a : catMaybes xs

-- | catMaybes implemented using a fold.
catMaybes' :: [Maybe a] -> [a]
catMaybes' xs = foldr f [] xs
    where
        f :: (Maybe a) -> [a] -> [a]
        f x ys = case x of
                    Nothing ->  ys
                    Just a -> a : ys


--------------------------------------------------------------------------------

-- | Given a list of pairs, returns the lower element for each pair.
minima :: Ord a => [(a,a)] -> [a]
minima [] = []
minima (x:xs) = (uncurry (min)) x : minima xs -- This first unpacks the pairs from the list , passes it into uncurry min and then appends using con.

--------------------------------------------------------------------------------

checkIfPairEqualSeven :: (Num a, Eq a) => a -> a -> Bool
checkIfPairEqualSeven a b = if (a + b) == 7 then True else False


-- | Keeps only the pairs of the input whose values sum to 7.
sumToSeven :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
sumToSeven [] = []
sumToSeven (x:xs) = case ((uncurry (checkIfPairEqualSeven))(x)) of
                        True -> x : sumToSeven xs
                        False -> sumToSeven xs


--------------------------------------------------------------------------------

-- | Pairs up the elements of two lists elementwise, until either list runs out.
zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip (x:xs) [] = []
zip [] (y:ys) = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


-- | Pairs up the elements of two lists elementwise, until either list runs out.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = []
zipWith f (x:xs) [] = []
zipWith f [] (y:ys) = []
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys 