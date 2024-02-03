{-# LANGUAGE FlexibleInstances #-}
module TypeClasses where

-- | Generate the string representation of all numbers up to the argument.
numberStringsTo :: Int -> [String]
numberStringsTo n = [show x | x <- [1..n]] 

-- | Returns True if there is a zero in the incoming list of space-separated 
-- integers.
anyZero :: String -> Bool
--anyZero str = not (null(filter (==0)(map read (words str)))) --First converts to ints, then filters for any 0s. If list empty, then no 0s else there is.
anyZero str = (or)(map (==0)(map read (words str))) -- Converts to ints, then map each value to True/False on whether it ==0 and then ors all of them.
-- | For a given number, returns the positive and negative versions of the 
-- number.
plusMinus :: (Num a) => a -> (a,a) --Added constraint 
plusMinus a = (-(abs a), (abs a)) 

-- | Returns True if and only if the element appears in the list.
appears :: (Eq a) => a -> [a] -> Bool
--appears a lst = (or)(map (==a)(lst))          --This also works
appears a lst = not (null (filter (==a)(lst))) 

-- | Gets the largest of the 3 numbers, using (<=) and related functions.
largest3 :: (Ord a) => a -> a -> a -> a
largest3 a b c = case (a >= b && a >= c) of
                    True -> a
                    False -> case (b >= a && b >= c) of
                                True -> b
                                False -> c


-- | Computes the AND of two boolean values via integer maths.
eAnd :: Bool -> Bool -> Bool
eAnd b1 b2 = toEnum ((*)(fromEnum b1)(fromEnum b2))

-- | Computes the OR of two boolean values via integer maths.
eOr :: Bool -> Bool -> Bool
eOr b1 b2 = toEnum (max (fromEnum b1) (fromEnum b2)) 


-- | Computes the NOT of a boolean value via integer maths.
eNot :: Bool -> Bool 
eNot b = toEnum (1 - fromEnum b) -- 1 - 1 = 0 and 1 - 0 = 1 so it works.

-----------------------------------------------------------------------------------

class Pog a where
    morePog :: a -> a -> Bool -- Returns true if the first argument is more pog than the second, else false
    mostPog :: a --the most pog element of the type

--Note that in the instance declaration below, the functions have been declared to however we want. 
instance Pog Bool where
    morePog True False = True --Must be True False for it to return true
    morePog _ _ = False --Else return false
    mostPog = True  --May retern an error in ghci if you do not specify the type when calling, as a is too generic. Call mostPog :: Bool in ghci

instance Pog Integer where
    mostPog = 60
    morePog a b = a > b -- This also works : if a > b then True else False

instance Pog String where
    mostPog = "POG STRING"
    morePog a b = length a > length b  --Returns true if a is more pog than b. Pog in this context is the length of the string.

instance (Pog a, Pog b) => Pog (a,b) where
    mostPog = (mostPog, mostPog)
    morePog (a,b) (c,d) = morePog a c && morePog b d

    -- if we run morePog mostPog True -> What this actually does is run morePog on the BOOL instance 
    -- and mostPog = True in that instance so it does morePog True True which returns False

    -- if we run morePog mostPog (0, ("Hello", True))
    -- what this actually does is morePog mostPog 0 && morePog (mostPog, mostPog) ("Hello", True)
    -- this then simplifies to morePog 60 0 && morePog mostPog "Hello" && morePog mostPog True
    -- this then simplifies to morePog 60 0 && morePog "POG STRING" "Hello" && morePog True True
    -- This then goes to True && True && False
    -- This is False
    
