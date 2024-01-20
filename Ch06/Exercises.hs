{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use splitAt" #-}
module Exercises where

import Prelude hiding (and, concat, drop, elem, even, init, odd, product, replicate, zip, (!!), (++), (^))
import GHC.Stack (callStack)

------------------------------------------------------------------------
--                          Exercise 6.1 (*)                          --
------------------------------------------------------------------------
-- How does the recursive version of the factorial function behave if applied to
-- a negative argument, such as (-1)? Modify the definition to prohibit negative
-- arguments by adding a guard to the recursive case.

-- The  function  does  not  terminate,  because  each  application  of  fac  decreases  the  argument  by  one,  and
-- hence the base case is never reached.

factorial :: Int -> Int
factorial n
  | n < 0 = error "factorial: negative argument"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

-- factorial (-1)

-- *** Exception: factorial: negative argument

-- factorial 0
-- 1

-- factorial 1
-- 1


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
2 ^ 3
= { applying ^ }
2 * (2 ^ 2)
= { applying ^ }
2 * (2 * (2 ^ 1))
= { applying ^ }
2 * (2 * (2 * (2 ^ 0)))
= { applying ^ }
2 * (2 * (2 * 1))
= { applying * }
8 
-}

------------------------------------------------------------------------
--                          Exercise 6.4 (*)                          --
------------------------------------------------------------------------
-- Define a recursive function euclid :: Int -> Int -> Int that implements
-- Euclid’s algorithm for calculating the greatest common divisor of two non-
-- negative integers: if the two numbers are equal, this number is the result;
-- otherwise, the smaller number is subtracted from the larger, and the same
-- process is then repeated. For example:
-- > euclid 6 27
-- 3

euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | otherwise = euclid (x - y) y




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

{-
length [1,2,3]
= { applying length }
= 1 + length [2,3]
= { applying length }
= 1 + 1 + length [3]
= { applying length }
= 1 + 1 + 1 + length []
= { applying length }
= 1 + 1 + 1 + 0 -- use base case
= { applying + }
= 3

drop 3 [1,2,3,4,5]
= { applying drop }
= drop (3 - 1) [2,3,4,5]
= { applying drop }
= drop (2 - 1) [3,4,5]
= { applying drop }
= drop (1 - 1) [4,5]
= { applying drop }
= drop 0 [4,5]
= { applying drop }
= [4,5] -- use base case

init [1,2,3]
= { applying init }
= 1 : init [2,3]
= { applying init }
= 1 : 2 : init [3]
= { applying init }
= 1 : 2 : []
= { applying : }
= [1,2] -- use base case which is [_] = []
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
and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && and bs
--and (b:bs) = b && (and bs)



-- (b) Concatenate a list of lists:
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ (concat xss)
--concat (xs:xss) = xs:(concat xss)


-- (c) Produce a list with a non-negative number of identical elements:
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:(replicate (n - 1) x)


-- (d) Select the nth element of a list:
(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
--(!!) [] _ = [] --has not to be defined would anyway be an error
(!!) (x:xs) n = (!!) xs (n-1)

-- (e) Decide if a value is an element of a list:
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

------------------------------------------------------------------------
--                          Exercise 6.9 (**)                         --
------------------------------------------------------------------------
-- Using the five-step process, construct the library functions that:
-- (a) calculate the sum of a list of numbers;
-- (b) take a given number of elements from the start of a list;
-- (c) select the last element of a non-empty list.

-- (a) calculate the sum of a list of numbers;
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- (b) take a given number of elements from the start of a list;
takeElements :: Int -> [a] -> [a]
takeElements _ [] = []
takeElements n (x:xs)
    | n <= 0 = []
    | otherwise = x : takeElements (n-1) xs

-- (c) select the last element of a non-empty list.
lastElement :: [a] -> a
lastElement [x] = x
lastElement (_:xs) = lastElement xs

------------------------------------------------------------------------
------------------------------------------------------------------------
--                          Book                       --
------------------------------------------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
--                          6.1 Basic concepts                        --
------------------------------------------------------------------------

-- factorial
fac :: Int -> Int
fac 0 = 1 -- base case
fac n = n * fac (n - 1) -- recursive case

-- not recursive
{-
fac :: Int -> Int
fac n = product [1 .. n]
-}

{-
fac 3
= { applying fac }
3 * fac 2
= { applying fac }
3 * (2 * fac 1)
= { applying fac }
3 * (2 * (1 * fac 0))
= { applying fac }
3 * (2 * (1 * 1))
= { applying * }
6
-}

---- multiplication operator *
{-
(*) :: Int -> Int -> Int
m * 0 = 0 -- base case
m * n = m + (m * (n - 1)) -- recursive case
-- formalises the idea that multiplication can
-- be reduced to repeated addition.
-}

{-
4 *' 3
= { applying *' }
4 + (4 *' 2)
= { applying *' }
4 + (4 + (4 *' 1))
= { applying *' }
4 + (4 + (4 + (4 *' 0)))
= { applying *' }
4 + (4 + (4 + 0))
= { applying + }
12
-}

------------------------------------------------------------------------
--                          6.2 Recursion on lists                    --
------------------------------------------------------------------------

-- 2:(3:(4:[])) = [2,3,4]

---- product: product [2,3,4] = 24
product' :: (Num a) => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

{-
product' [2,3,4]
= { applying product' }
2 * product' [3,4]
= { applying product' }
2 * (3 * product' [4])
= { applying product' }
2 * (3 * (4 * product' []))
= { applying product' }
2 * (3 * (4 * 1))
= { applying * }
24
-}

---- length: length [2,3,4] = 3
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

{-
length' [2,3,4]
= { applying length' }
1 + length' [3,4]
= { applying length' }
1 + (1 + length' [4])
= { applying length' }
1 + (1 + (1 + length' []))
= { applying length' }
1 + (1 + (1 + 0))
= { applying + }
3
-}

---- reverse : reverse [1,2,3] = [3,2,1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

{-
reverse' [1,2,3]
= { applying reverse' }
reverse' [2,3] ++ [1]
= { applying reverse' }
(reverse' [3] ++ [2]) ++ [1]
= { applying reverse' }
((reverse' [] ++ [3]) ++ [2]) ++ [1]
= { applying reverse' }
(([] ++ [3]) ++ [2]) ++ [1]
= { applying ++ }
([3] ++ [2]) ++ [1]
= { applying ++ }
[3,2] ++ [1]
= { applying ++ }
[3,2,1]
-}

---- ++ operator : [1,2,3] ++ [4,5] = [1,2,3,4,5]
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys -- base case
(x : xs) ++ ys = x : (xs ++ ys) -- recursive case
{-
[1,2,3] ++ [4,5]
= { applying ++ }
1 : ([2,3] ++ [4,5])
= { applying ++ }
1 : (2 : ([3] ++ [4,5]))
= { applying ++ }
1 : (2 : (3 : ([] ++ [4,5])))
= { applying ++ }
1 : (2 : (3 : [4,5]))
= { applying : }
[1,2,3,4,5]
-}

---- insert: insert 3 [1,2,4,5] = [1,2,3,4,5]
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x] -- base case
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys -- recursive case
  {-
  insert 3 [1,2,4,5]
  = { applying insert }
  1 : insert 3 [2,4,5]
  = { applying insert }
  1 : (2 : insert 3 [4,5])
  = { applying insert }
  1 : (2 : (3 : [4,5]))
  = { applying : }
  [1,2,3,4,5]
  -}

---- insertion sort: isort [3,2,1,4] = [1,2,3,4]
isort :: (Ord a) => [a] -> [a]
isort [] = [] -- base case
isort (x : xs) = insert x (isort xs) -- recursive case
{-
isort [3,2,1,4]
= { applying isort }
insert 3 (insert 2 (insert 1 (insert 4 [])))
= { applying insert }
insert 3 (insert 2 (insert 1 [4]))
= { applying insert }
insert 3 (insert 2 [1,4])
= { applying insert }
insert 3 [1,2,4]
= { applying insert }
[1,2,3,4]
-}

------------------------------------------------------------------------
--                          6.3 Multiple arguments                   --
------------------------------------------------------------------------

---- zip: zip [’a’,’b’,’c’] [1,2,3,4]
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = [] -- base case
zip _ [] = [] -- base case
zip (x : xs) (y : ys) = (x, y) : zip xs ys -- recursive case
{-
zip [’a’,’b’,’c’] [1,2,3,4]
= { applying zip }
(’a’,1) : zip [’b’,’c’] [2,3,4]
= { applying zip }
(’a’,1) : (’b’,2) : zip [’c’] [3,4]
= { applying zip }
(’a’,1) : (’b’,2) : (’c’,3) : zip [] [4]
= { applying zip }
(’a’,1) : (’b’,2) : (’c’,3) : []
= { list notation }
[(’a’,1), (’b’,2), (’c’,3)]
-}

---- drop: drop 3 [1,2,3,4,5] = [4,5]
drop :: Int -> [a] -> [a]
drop 0 xs = xs -- base case
drop _ [] = [] -- base case
drop n (_ : xs) = drop (n - 1) xs -- recursive case
{-
drop 3 [1,2,3,4,5]
= { applying drop }
drop 2 [2,3,4,5]
= { applying drop }
drop 1 [3,4,5]
= { applying drop }
drop 0 [4,5]
= { applying drop }
[4,5]
-}

------------------------------------------------------------------------
--                          6.4 Multiple recursion                   --
------------------------------------------------------------------------

---- n-Fibonacci numbers: fib 3 = 2
fib :: Int -> Int
fib 0 = 0 -- base case
fib 1 = 1 -- base case
fib n = fib (n - 1) + fib (n - 2) -- recursive case
{-
fib 3
= { applying fib }
fib (3 - 1) + fib (3 - 2)
= { applying fib }
fib 2 + fib (3 - 2)
= { applying fib }
(fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
= { applying fib }
(fib 1 + fib (2 - 2)) + fib (3 - 2)
= { applying fib }
(1 + fib (2 - 2)) + fib (3 - 2)
= { applying fib }
(1 + fib 0) + fib (3 - 2)
= { applying fib }
(1 + 0) + fib (3 - 2)
= { applying + }
1 + fib (3 - 2)
= { applying fib }
1 + fib 1
= { applying fib }
1 + 1
= { applying + }
2
-}

---- quick sort: qsort [3,5,1,4,2] = [1,2,3,4,5]
qsort :: (Ord a) => [a] -> [a]
qsort [] = [] -- base case
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger -- recursive case
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

{-
qsort [3,5,1,4,2]
= { applying qsort }
qsort [1,2] ++ [3] ++ qsort [5,4]
= { applying qsort }
(qsort [] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ qsort [])
= { applying qsort }
([] ++ [1] ++ [2]) ++ [3] ++ ([4] ++ [5] ++ [])
= { applying ++ }
[1,2] ++ [3] ++ [4,5]
= { applying ++ }
[1,2,3,4,5]
-}

------------------------------------------------------------------------
--                          6.5 Mutual recursion                     --
------------------------------------------------------------------------

---- Define even and odd: even 4 = True
even :: Int -> Bool
even 0 = True -- base case
even n = odd (n - 1) -- recursive case

odd :: Int -> Bool
odd 0 = False -- base case
odd n = even (n - 1) -- recursive case

{-
even 4
= { applying even }
odd 3
= { applying odd }
even 2
= { applying even }
odd 1
= { applying odd }
even 0
= { applying even }
True
-}

--- Define evens and odds: evens "abcde" = "ace"
evens :: [a] -> [a]
evens [] = [] -- base case
evens (x : xs) = x : odds xs -- recursive case

odds :: [a] -> [a]
odds [] = [] -- base case
odds (_ : xs) = evens xs -- recursive case

-- "abcde" is just an abbreviation for [’a’,’b’,’c’,’d’,’e’]

{-
evens "abcde"
= { applying evens }
’a’ : odds "bcde"
= { applying odds }
’a’ : evens "cde"
= { applying evens }
’a’ : ’c’ : odds "de"
= { applying odds }
’a’ : ’c’ : evens "e"
= { applying evens }
’a’ : ’c’ : ’e’ : odds []
= { applying odds }
’a’ : ’c’ : ’e’ : []
= { string notation }
"ace"
-}

------------------------------------------------------------------------
--                          6.6 Advice on recursion                  --
------------------------------------------------------------------------

{-
Step 1: define the type
Step 2 : enumerate the cases
Step 3 : define the simple cases
Step 4 : define the other cases
Step 5 : generalise and simplify
-}