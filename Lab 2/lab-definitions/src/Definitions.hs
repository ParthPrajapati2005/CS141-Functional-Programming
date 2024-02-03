module Definitions where

{-
NOTE FROM ALEX:

The comments in lab sheets are just helpers to remind you what you have done.

You should still download and read the lab sheet - it has other things
that you need to know, and more exercises to complete!

Have fun :)
-}

--------------------------------------------------------------------------------

-- Implement `not` using the four different syntaxes we have seen.

notIf, notCase, notTLP, notGuard :: Bool -> Bool

-- if...then...else...
notIf bool = if bool == True then False else True   --Invert function using if - then - else

-- case...of...
notCase bool' = case bool' of {
                    True -> False;
                    False -> True;
                }

-- top level pattern matching
notTLP True = False
notTLP False = True

-- guards
notGuard bool''
            | bool'' == True    = False
            | otherwise         = True

--------------------------------------------------------------------------------
{-
    For the not function, the best syntax would be top level pattern matching. This is because there are only 2 outcomes. 
    The least appropriate syntax would be the guards. It has a more complex syntax. To personal preferencem the case of is 
    my personal favorite. 
-}
--------------------------------------------------------------------------------
-- Implement the factorial function.

fac :: Integer -> Integer
fac n = case n of{
            0 -> 1;
            _ -> n * fac(n-1);
        }

--------------------------------------------------------------------------------


-- Here is a weird function called interesting:
interesting :: (p -> q) -> p -> q
interesting a b = a b

-- Write interesting as a lambda function.
interestingLambda :: (p -> q) -> p -> q
interestingLambda = \a -> \b -> a b

-- Write interesting as an operator called £.
infixr 0 £
(£) :: (p -> q) -> p -> q
(£) a b = a b


--------------------------------------------------------------------------------

-- Here is a function called foo:
foo :: Int -> Int -> Int
foo x y = if x > y then x - y else y - x

-- Write foo as a lambda function.
fooLambda :: Int -> Int -> Int
fooLambda = \x -> \y -> if x > y then x - y else y - x 

-- Golf the definition of foo.
fooGolfed :: Int -> Int -> Int
fooGolfed x y = case (x > y) of{
                    True -> x - y;
                    False -> y - x;
                } 