module L2 where --This needs to match the name of the file
import Bar

hi = "Hello World!"        --Using Strings

--Declarations and Functions

triangle n = sum [1..n] --A declaration (a function called triangle with n as parameter). Finds sum from 1 to n
quadruple x = double(x) -- Uses function declared in Bar.hsLibraries

max3 x y z = max(max x y) z --Brackets only used for ordering - finds the max of 3 numbers

--Lambdas

myMin' x y = if x < y then x else y
myMin'' x = \y -> if x < y then x else y      ---All 3 of these statements are the same.
-- myMin''' = \x -> \y -> if x < y then x else y 
interestingLambda :: (p -> q) -> p -> q
interestingLambda = \a -> \b -> a b


foo x y = x y
--Partial Funtions

minimumOfFiveAnd = min 5  --Partial Funtion - calculates the minimum of 5 and something

--Operators
x ^^^ y = max x y  --When in ghci you can do e.g 3^^^4 and will give the max of 3 and 4 => 4 
                   --You can also do (^^^) 3 4 and it will still work

--Recursive funtions
factorial x
    |x==0   =1
    |otherwise = x * factorial(x-1) --INDENTATION 

fac' x = case x of { --Pattern matching
    0 ->1; 
    _ -> x*fac'(x-1);} -- Anothe version of the same function.