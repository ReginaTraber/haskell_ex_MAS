module Exercises where

import Prelude hiding (replicate)

------------------------------------------------------------------------
--                          Exercise 5.1 (**)                         --
------------------------------------------------------------------------
-- Using a list comprehension, give an expression that calculates the sum 1^2 +
-- 2^2 + . . . + 100^2 of the first one hundred integer squares. You may use the
-- function sum :: [Int] -> Int in your definition.

e5_1 :: Int
e5_1 = sum [x^2 | x <- [1..100]]


------------------------------------------------------------------------
--                          Exercise 5.4 (*)                          --
------------------------------------------------------------------------
-- In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a]
-- that produces a list of identical elements can be defined using a list
-- comprehension. For example:
-- > replicate 3 True
-- [True,True,True]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]


------------------------------------------------------------------------
--                          Exercise 5.8 (**)                         --
------------------------------------------------------------------------
-- Redefine the function positions
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- without using list comprehension, but using the functions find and zip.

-- zip is provided by the Standard Prelude:
-- zip :: [a] -> [b] -> [(a,b)]
-- zip [] _ = []
-- zip _ [] = []
-- zip (a:as) (b:bs) = (a,b) : zip as bs

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
--positions x xs = find x (zip xs [0..])
positions x xs = x `find` (xs `zip` [0..])
