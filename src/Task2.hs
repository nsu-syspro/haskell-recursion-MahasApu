{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)


-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum, luhnN, doubleEveryOther, toDigits, normalize)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n f x = luhnN (map f x) n

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id 

-----------------------------------
--
-- Computes Luhn check digit for an integer by converting it to digits and using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> lunhFunctionDec 3456
-- 1

lunhFunctionDec :: Integer -> Int
lunhFunctionDec x = luhnDec (toDigits x)

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt 

-----------------------------------
-- Checks if a character is a decimal digit
--
-- Usage example:
-- >>> isDigit '5'
-- True
-- >>> isDigit 'a'
-- False

isDigit :: Char -> Bool
isDigit x = (x >= '0') && (x <= '9')

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt x 
  | isDigit x                = fromEnum x - fromEnum '0'
  | (x >= 'a') && (x <= 'f') = fromEnum x - fromEnum 'a' + 10
  | (x >= 'A') && (x <= 'F') = fromEnum x - fromEnum 'A' + 10
  | otherwise = error "Char is not convertable to Int!"

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec x = lunhFunctionDec (div x 10) == fromInteger (mod x 10)


-----------------------------------
-- Returns the last element of a list
--
-- Usage example:
-- >>> lastElement [1,2,3,4]
-- 4

lastElement :: [a] -> a
lastElement [ ]    = error "Empty list! Last element does not exist."
lastElement [x]    = x 
lastElement (_:xs) = lastElement xs 


-----------------------------------
-- Returns all elements of a list except the last one
--
-- Usage example:
-- >>> init [1,2,3,4]
-- [1,2,3]
-- >>> init "abcd"
-- "abc"

init :: [a] -> [a]
init [ ]    = [ ]         
init [_]    = [ ]        
init (x:xs) = x : init xs  

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex x = luhnHex (init x) == digitToInt (lastElement x)
