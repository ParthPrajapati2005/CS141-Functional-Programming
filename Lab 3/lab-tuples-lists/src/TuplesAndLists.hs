module TuplesAndLists where
import Prelude hiding (curry, uncurry, null)

--------------------------------------------------------------------------------
-- PAIRS
pair :: a -> b -> (a, b)
pair x y = (x, y)

swap :: (a,b) -> (b,a)
swap (a, b) = (b, a)

--------------------------------------------------------------------------------
-- TRIPLES

birthday :: (Int, Int, Int)
birthday = (13, 07, 2005)

today :: (Int,Int,Int)
today = (11, 01, 2024)

age :: (Int,Int,Int) -> (Int,Int,Int) -> Int
age (bd,bm,by) (td,tm,ty) = 
    case (tm > bm || ((tm == bm) && td >= bd)) of
        True -> (ty - by)
        False -> (ty - by - 1)
                        

--------------------------------------------------------------------------------
-- LISTS

oneTwoThree :: [Int]
-- oneTwoThree = (1:(2:(3 : []))) --First way
-- oneTwoThree = 1 : 2 : 3 : []   --Second Way
oneTwoThree = [1,2,3]             --Third Way

null :: [a] -> Bool
--null [] = True         
--null (x:xs) = False

null a = case a of
            [] -> True
            (x:xs) -> False

isPalindrome :: Eq a => [a] -> Bool
isPalindrome (a) = ((reverse a) == a) -- Check if palindrome

sayTimes :: String -> Int -> String
sayTimes (str) x = concat(replicate x str)

--COMPLETE!