-- {-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-----------------------------------
--
-- Checks whether the last digit is a valid check digit
-- for the rest of the given number using Luhn algorithm
--
-- Usage example:
--
-- >>> validate 3456
-- False
-- >>> validate 34561
-- True
-- >>> validate 34562
-- False

validate :: Integer -> Bool
validate x = lunhFunction (div x 10) == fromInteger (mod x 10)

-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1

luhnFormula :: Int -> Int
luhnFormula x = mod (10 - mod x 10) 10

luhn :: [Int] -> Int
luhn x = luhnFormula (sum (map normalize (doubleEveryOther (reverse x))))

lunhFunction :: Integer -> Int
lunhFunction x = luhn (toDigits x)
-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for zero and negative numbers) returns empty list
--
-- Usage example:
--
-- >>> toDigits 3456
-- [3,4,5,6]
-- >>> toDigits 0
-- [0]
-- >>> toDigits (-123)
-- []

isDigit :: Integer -> Bool
isDigit x = x < 10

toDigits :: Integer -> [Int]
toDigits x 
    | x <= 0      = [ ]
    | isDigit x = [fromIntegral x]
    | otherwise   = toDigits (div x 10) ++ [fromIntegral (mod x 10)]

-----------------------------------
--
-- Produces list in reverse order to the given one
--
-- Usage example:
--
-- >>> reverse "Hello"
-- "olleH"
-- >>> reverse [3,4,5,6]
-- [6,5,4,3]

reverse :: [a] -> [a]
reverse [ ]      = [ ]
reverse (x : xs) = reverse xs ++ [x]
 
-----------------------------------
--
-- Doubles every other digit starting from first one
--
-- Usage example:
--
-- >>> doubleEveryOther [6,5,4,3]
-- [12,5,8,3]

doubleIfEvenIndex :: (Int, Int) -> Int
doubleIfEvenIndex (i, x) = if even i then x * 2 else x

addIndices :: [Int] -> [(Int, Int)]
addIndices = zip [0..]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = map doubleIfEvenIndex (addIndices xs)

-----------------------------------
--
-- Normalizes given number to single digit by subtracting 9
-- if it is greater than or equal to 10
--
-- (Assumes inputs between 0 and 18)
--
-- Usage example:
--
-- >>> normalize 12
-- 3
-- >>> normalize 1
-- 1

normalize :: Int -> Int
normalize x = if x >= 10 then x - 9 else x

-----------------------------------
--
-- Produces list with given function applied to each element
-- in given list
--
-- Usage example:
--
-- >>> map (\x -> x * 2) [1,2,3,4]
-- [2,4,6,8]

map :: (a -> b) -> [a] -> [b]
map _ [ ]         = [ ]
map func (x : xs) = func x : map func xs

-----------------------------------
--
-- Computes sum of given list of numbers
--
-- Usage example:
--
-- >>> sum [3,8,5,3]
-- 19
-- >>> sum []
-- 0

sum :: [Int] -> Int
sum [ ]      = 0
sum [x]      = x
sum (x : xs) = x + sum xs 

