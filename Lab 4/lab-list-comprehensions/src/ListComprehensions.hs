module ListComprehensions where
import Data.List
--------------------------------------------------------------------------------

-- Returns True iff the first argument can be divided by the second with no remainder.
dividesBy :: Integer -> Integer -> Bool
dividesBy x y = (x `mod` y) == 0


-- Whether or not the argument is prime.
isPrime :: Integer -> Bool
isPrime n = (n > 1) && not ((or) [(dividesBy n x) | x <- [2..floor(sqrt(fromIntegral n))]])


-- The infinite list of all prime numbers.
primes :: [Integer]
primes = filter (\p -> isPrime p) [x | x <- [0..]]


-- All possible outcomes of rolling two die, with number of sides m and n, including duplicates.
rollOutcomes :: Int -> Int -> [Int]
rollOutcomes m n = [x | x <- [y+z | y <- [1..m], z <- [1..n]]]


-- The Cartesian product of two lists.
cartProd :: [a] -> [b] -> [(a,b)]
cartProd x y = [(a,b) | a <- x, b <- y]


-- Given a function and two lists, apply f to every pair of elements from the two lists.
cartProdWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartProdWith someFunc x y  = [someFunc a b | a <- x, b <- y ]

-- Implement rollOutcomes again, using the cartProdWith function defined above.
rollOutcomes' :: Int -> Int -> [Int]
rollOutcomes' m n = cartProdWith (+) [1..m] [1..n]

-- The full lowercase alphabet.
letters :: [Char]
letters = ['a'..'z']

-- The five lowercase vowels.
vowels :: [Char]
vowels = "aeiou"


-- The twenty-one lowercase consonants.
consonants :: [Char]
consonants = letters \\ vowels


-- A function which gives back "FizzBuzz" if the number is a multiple of 3 and 5; "Fizz" if only a multiple of 3; "Buzz" if only a multiple of 5; and the number as a string otherwise.
fb :: Integer -> String
fb x = case (x `mod` 3 == 0 && x `mod` 5 == 0) of
            True -> "FizzBuzz"
            False -> case (x `mod` 3 == 0 || x `mod` 5 == 0) of
                        False -> show x
                        True -> case (x `mod` 3 == 0) of
                                    True -> "Fizz"
                                    False -> "Buzz"

-- Using `map`, compute the full FizzBuzz from 1 up to the argument given.
fizzbuzzTo :: Integer -> [String]
fizzbuzzTo x = map fb [1..x] 

-- How many times is "Fizz" printed between 1 and 1000?
howManyFizzes :: Int
howManyFizzes = length (filter (\f -> (f=="Fizz")) (fizzbuzzTo 1000))