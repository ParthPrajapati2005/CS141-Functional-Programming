----------------------------- MODULE NAME --------------------------------
module L3 where
----------------------------- DATA TYPES ---------------------------------

--In Haskell , EVERY expression has a TYPE

someBoolean :: Bool --Declararation of Data Type
someBoolean = True && False || True

five :: Integer --Declararation of Data Type
five = min 5 6 

hello :: String
hello = "Hello World" 

h :: Char
h = 'H' --Char

--If you do not specify a type, the compiler is capable of identifying the types itself. It is good practice to define the types.
{-So what is the point of declaring types if the compiler can do it for us?
    To avoid TYPE ERRORS
    HASKELL is a STRONGLY TYPED, lazy, purely-functional programming language
-}
--Types only exist at runtime. As soon as the type checker finishes, it erases all of the types. This means we cannot check the type of a value at runtime.
--Because the type checker can tell us when things are wrong, we can use it as a tool to make sure we are writing good code

--------------------------------- FUNCTION TYPES --------------------------------
--Consider a mathematical funtion which maps the integers to the integers e.g succ(x) : R -> R succ(x) = x + 1
f :: Integer -> Integer
f n = n + 1

--------------------------------- POLYMORPHISM ----------------------------------
g :: a -> a  --This is a general type and it must return the same data type as the input
g x = x

ignoreSecondArgument :: p1 -> p2 -> p1 --The type declaration can get confusing, but just remember the last one is the output and all others are inputs
ignoreSecondArgument x y = x --This ignores the second argument and returns the first one
