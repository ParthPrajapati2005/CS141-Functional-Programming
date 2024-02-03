module DataTypes where
--------------------------------------------------------------------------------
-- Exercise 1: Grade

          -- We put the values in this order because Ord type class says that values to the left are considered less than values to the right.
data Grade = Fail | Third | TwoTwo | TwoOne | First deriving (Eq, Show, Ord) -- This means we can use the stock operarations of the Eq & Show Type class on these values. 


toGrade :: Int -> Grade
toGrade intGrade
      | intGrade >=70  = First
      | intGrade >= 60 = TwoOne
      | intGrade >= 50 = TwoTwo
      | intGrade >= 40 = Third
      | otherwise      = Fail

getBoundaries :: Grade -> (Int, Int)
getBoundaries grade
      | grade == First  = (70,100)
      | grade == TwoOne = (60,69)
      | grade == TwoTwo = (50,59)
      | grade == Third  = (40,49)
      | grade == Fail   = (0,39)

toGrade' :: Int -> Maybe Grade
toGrade' mark
        | (mark >= 0) && (mark <= 100) = Just (toGrade mark) 
        | otherwise = Nothing


--------------------------------------------------------------------------------
-- Exercise 2: Shapes

data Circle = Circle Float deriving (Eq, Show)

class Shape a where
  perimeter :: a -> Float
  area      :: a -> Float
  unit      :: a

instance Shape Circle where
  perimeter (Circle r) = 2 * pi * r
  area      (Circle r) = r * r * pi
  unit = Circle 1 -- Define the unit circle to be a circle with radius 1.


{-
Consider the following code : perimeter unit

This will give use the perimeter of the unit... what? Which unit is it going to pick? It could be the
unit circle, or triangle, or anything else. This code is ambiguous.

To specify the type we are talking about, we can supply a typing for it. So we can write this:

perimeter (unit :: Circle) - This specifies that we want the unit definition for the Circle type. 
-}
----------------------------------------
data Triangle = Triangle Float Float Float deriving (Eq, Show)

instance Shape Triangle where
  perimeter (Triangle a b c) = a + b + c 

  area (Triangle a b c) = 
    let 
      s = (perimeter (Triangle a b c) / 2)
    in
      sqrt (s * (s - a) * (s - b) * (s - c))

  unit = Triangle 1 1 1 

----------------------------------------
data Quad = Square Float | Rectangle Float Float deriving (Eq, Show)

instance Shape Quad where
  perimeter (Square x) = x * 4
  perimeter (Rectangle x y) = (2*x) + (2*y)

  area (Square x) = x * x
  area (Rectangle x y) = x * y 
  
  unit = Square 1
-- Put your Shape instance for Quad here
