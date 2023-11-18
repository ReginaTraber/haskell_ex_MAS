{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Exercises where

import Prelude hiding ((^), and, concat, replicate, (!!), elem)


------------------------------------------------------------------------
--                          Exercise 6.2 (*)                          --
------------------------------------------------------------------------
-- Define a recursive function sumdown :: Int -> Int that returns the sum of the
-- non-negative integers from a given non-negative integer to zero. For example,
-- sumdown 3 should return the result 3 + 2 + 1 + 0 = 6.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

------------------------------------------------------------------------
--                          Exercise 6.3 (*)                          --
------------------------------------------------------------------------
-- Redefine the exponentiation operator ^ for non-negative integers using the same
-- pattern of recursion as the multiplication operator *, and show how the
-- expression 2 ^ 3 is evaluated using your definition.

(^) ::  Int -> Int -> Int
(^) _ 0 = 1 -- base case
--(^) 0 _ = 0 --to break earlier (optional)
(^) n e = n * ((^) n (e - 1))

{-
2^3
= 2 * (2^2)
= 2 * (2 * (2^1))
= 2 * (2 * (2 * (2^0)))
= 2 * (2 * (2 * 1) * 1) --2^0 = 1 base case
-}

------------------------------------------------------------------------
--                          Exercise 6.5 (**)                         --
------------------------------------------------------------------------
-- Using the recursive definitions given in chapter ”Recursive Functions”,
-- show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.

-- length :: [a] -> Int
-- length [] = 0
-- length (_ : xs) = 1 + length xs

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_ : xs) = drop (n - 1) xs

-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x : init xs

-- No executable code required.
-- Enter your solution within the block comment below.

{-
length [1,2,3]
= 1 + length [2,3]
= 1 + 1 + length [3]
= 1 + 1 + 1 + length []
= 1 + 1 + 1 + 0 -- use base case
= 3

drop 3 [1,2,3,4,5]
= drop (3 - 1) [2,3,4,5]
= drop (2 - 1) [3,4,5]
= drop (1 - 1) [4,5]
= drop 0 [4,5]
= [4,5] -- use base case

init [1,2,3]
= 1 : init [2,3]
= 1 : 2 : init [3]
= 1 : 2 : []
= [1,2] --use base case which is [_] = []
-}


------------------------------------------------------------------------
--                          Exercise 6.6 (**)                         --
------------------------------------------------------------------------
-- Without looking at the definitions from the standard prelude, redefine the
-- following library functions on lists using recursion.  Note: Most of these
-- functions are defined in the prelude using other library functions rather
-- than using explicit recursion, and are generic functions rather than being
-- specific to the type of lists.

-- (a) Decide if all logical values in a list are True:
--     and :: [Bool] -> Bool


and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && and bs
--and (b:bs) = b && (and bs)



-- (b) Concatenate a list of lists:
--     concat :: [[a]] -> [a]

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ (concat xss)
--concat (xs:xss) = xs:(concat xss)


-- (c) Produce a list with a non-negative number of identical elements:
--     replicate :: Int -> a -> [a]

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:(replicate (n - 1) x)


-- (d) Select the nth element of a list:
--     (!!) :: [a] -> Int -> a

(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
--(!!) [] _ = [] --has not to be defined would anyway be an error
(!!) (x:xs) n = (!!) xs (n-1)

-- (e) Decide if a value is an element of a list:
--     elem :: Eq a => a -> [a] -> Bool

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem k (x:xs) = k == x || elem k xs

------------------------------------------------------------------------
--                          Exercise 6.7 (**)                         --
------------------------------------------------------------------------
-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges
-- two sorted lists to give a single sorted list. For example:
-- > merge [2 ,5 ,6] [1 ,3 ,4]
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists such as
-- insert or isort, but should be defined using explicit recursion.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------------------------------------------------------
--                          Exercise 6.8 (**)                         --
------------------------------------------------------------------------

-- Using merge, define a function msort :: Ord a => [a] -> [a] that implements
-- merge sort, in which the empty list and singleton lists are already sorted,
-- and any other list is sorted by merging together the two lists that result
-- from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list
-- into two halves whose lengths differ by at most one.


halve :: [a] -> ([a], [a])
halve = \xs -> (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = [] --an empty list is already sorted
msort [x] = [x] --a singleton list is already sorted
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))