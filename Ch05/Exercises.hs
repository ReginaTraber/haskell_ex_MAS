{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isAsciiLower" #-}
module Exercises where

import System.IO (localeEncoding)
import Prelude hiding (concat, length, replicate, zip)

-- import Exercises (test)

------------------------------------------------------------------------
--                          Exercise 5.1 (**)                         --
------------------------------------------------------------------------
-- Using a list comprehension, give an expression that calculates the sum 1^2 +
-- 2^2 + . . . + 100^2 of the first one hundred integer squares. You may use the
-- function sum :: [Int] -> Int in your definition.

e5_1 :: Int
e5_1 = sum [x^2 | x <- [1..100]]

-- sum [1..10]
-- 55

-- sum [4.1, 2.0, 1.7]
-- 7.8

------------------------------------------------------------------------
--                          Exercise 5.2 (**)                         --
------------------------------------------------------------------------
-- Suppose that a coordinate grid of size m × n is given by the list of all pairs
-- (x,y) of integers such that 0 <= x <= m and 0 <= y <= n. Using a list
-- comprehension, define a function grid :: Int -> Int -> [(Int,Int)] that
-- returns a coordinate grid of a given size. For example:
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

-- Note: This is a list of tuples, not a tuple of lists.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

------------------------------------------------------------------------
--                          Exercise 5.3 (**)                         --
------------------------------------------------------------------------
-- Using a list comprehension and the function grid above, define a function
-- square :: Int -> [(Int,Int)] that returns a coordinate square of size n,
-- excluding the diagonal from (0,0) to (n,n). For example:
-- > square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

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
--                          Exercise 5.5 (**)                         --
------------------------------------------------------------------------
-- A triple (x,y,z) of positive integers is Pythagorean if it satisfies the
-- equation x^2 + y^2 = z^2. Using a list comprehension with three generators,
-- define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of
-- all such triples whose components are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-----------------------------------------------------------------------
--                          Exercise 5.6 (**)                         --
------------------------------------------------------------------------
-- A positive integer is perfect if it equals the sum of all of its factors,
-- excluding the number itself. Using a list comprehension and the function
-- factors, define a function perfects :: Int -> [Int] that returns the list
-- of all perfect numbers up to a given limit. For example:
-- > perfects 500
-- [6,28,496]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- mod :: Int -> Int -> Int

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum (init (factors x))]

-- init :: [a] -> [a]
-- init [1, 2, 3]
-- [1, 2]

-- sum :: Num a => [a] -> a
-- sum [1 .. 10]
-- 55

------------------------------------------------------------------------
--                          Exercise 5.7 (**)                         --
------------------------------------------------------------------------
-- Show how the single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- with two generators can be re-expressed using two comprehensions with
-- single generators. Hint: make use of the library function concat and nest
-- one comprehension within the other.

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (xs:xss) = xs ++ concat xss

e5_7 :: [(Int, Int)]
e5_7 = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

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


------------------------------------------------------------------------
--                          Exercise 5.9 (**)                         --
------------------------------------------------------------------------
-- The scalar product of two lists of integers xs and ys of length n is given
-- by the sum of the products of corresponding integers:
-- sum (xsi * ysi) for i <- [0..n-1]
-- In a similar manner to chisqr, show how a list comprehension can be used to
-- define a function scalarproduct :: [Int] -> [Int] -> Int that returns the
-- scalar product of two lists. For example:
-- > scalarproduct [1,2,3] [4,5,6]
-- 32

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

-- sum [1..10]
-- 55

-- sum [4.1, 2.0, 1.7]
-- 7.8

-- zip [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]

-- zip [1] ['a', 'b']
-- [(1,'a')]

------------------------------------------------------------------------
--                          Exercise 5.10 (**)                        --
------------------------------------------------------------------------
-- Modify the Caesar cipher program to also handle upper-case letters.

-- Not done

------------------------------------------------------------------------
------------------------------------------------------------------------
--                          Book                                      --
------------------------------------------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
--                          5.1 Basic concepts                        --
------------------------------------------------------------------------
-- list of all numbers between 1 and 5 squared
test1 :: (Num a, Enum a) => [a]
test1 = [x ^ 2 | x <- [1 .. 5]]

-- test1 = [1,4,9,16,25]

-- list of all possible pairings of an element from the list
-- [1, 2, 3] with an element from the list [4, 5]
test2 :: (Num a, Enum a) => [(a, a)]
test2 = [(x, y) | x <- [1, 2, 3], y <- [4, 5]]

-- test2 = [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- list of all possible pairings of an element from the list
-- [4, 5] with an element from the list [1, 2, 3]
test3 :: (Num a, Enum a) => [(a, a)]
test3 = [(x, y) | y <- [4, 5], x <- [1, 2, 3]]

-- test3 = [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

-- concatenates a list of lists
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- concat [[1,2,3],[4,5],[6]] = [1,2,3,4,5,6]
-- concat [[]] = []

-- selects all the first components from a list of pairs
firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

-- the generator _ <- xs simply serves as a counter to govern
-- the production of the appropriate number of ones
-- firsts [(1,2),(3,4)] = [1,3]
-- firsts [(1,2)] = [1]
-- firsts [] = []

-- length of a list
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- length [1,2,3] = 3
-- length [] = 0

------------------------------------------------------------------------
--                          5.2 Guards                                 --
------------------------------------------------------------------------

---- list of all numbers between 1 and 10 that are even
test4 :: [Int]
test4 = [x | x <- [1 .. 10], even x]

-- test4 = [2,4,6,8,10]

---- a function that maps a positive integer to its list of positive factors
factors' :: Int -> [Int]
factors' n = [x | x <- [1 .. n], n `mod` x == 0]

-- factors' 15 = [1,3,5,15]
-- factors' 7 = [1,7]
-- factor 0 = error
-- factors' (-5) = []

{-
mod :: Integral a => a -> a -> a
mod x y = x - y * (x `div` y)
-- mod 10 0 = *** Exception: divide by zero
-- mod 10 3 = 1
-- 10 `mod` 3 = 1
-}

---- a function that decides if a number is prime
prime :: Int -> Bool
prime n = factors' n == [1, n]

-- prime 15 = False --lazy evaluation is used
-- prime 7 = True

----produces the list of all prime numbers up to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

-- primes 40 = [2,3,5,7,11,13,17,19,23,29,31,37]
-- prime -1 = errror
-- prime 0 = False

---returns the list of all values that are associated with a given key in a table
find' :: (Eq a1) => a1 -> [(a1, a2)] -> [a2]
find' k t = [v | (k', v) <- t, k == k']

-- find' 'b' [('a',1),('b',2),('c',3),('b',4)] = [2,4]
-- find' 'd' [('a',1),('b',2),('c',3),('b',4)] = []
-- find' 1 [(1,'a'),(2,'b'),(3,'c'),(2,'d')] = ['a']

------------------------------------------------------------------------
--                          5.3 The zip function                       --
------------------------------------------------------------------------

---- a new list by pairing successive elements from two existing lists until
---- either or both lists are exhausted
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- zip [’a’,’b’,’c’] [1,2,3,4] = [(’a’,1),(’b’,2),(’c’,3)]

---- returns the list of all pairs of adjacent (benachbarte) elements from a list
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- pairs [1,7,3,1,5] = [(1,7),(7,3),(3,1),(1,5)]
{-
tail :: [a] -> [a]
tail (_ : xs) = xs
-- tail [1,2,3] = [2,3]
-}

---- decides if a list of elements of any ordered type is
----sorted by simply checking that all pairs of adjacent elements
----from the list are in the correct order
sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- sorted [1,2,3,4] = True
-- sorted [1,3,2,4] = False --lazy evaluation is used
{-
and :: [Bool] -> Bool
and [] = True
and (b : bs) = b && and bs
-- and [True,True,False] = False
-- and [True,True,True] = True
-}

---- returns the list of all positions at which a value occurs in a list
positions' :: (Eq a) => a -> [a] -> [Int]
positions' x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- positions' 'a' "abracadabra" = [0,3,5,7,10]
-- positions' 1 [1,2,3,1,4] = [0,3]
-- positions' True [False,True,False,True] = [1,3]
-- positions' 1 [] = []
-- positions' 1 [2,3,4] = []

------------------------------------------------------------------------
--                          5.4 String comprehensions                  --
------------------------------------------------------------------------

---- returns the number of lower-case letters in a string
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- lowers "Haskell" = 6

---- returns the number of a particular character in a string
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- count 's' "Mississippi" = 4

{-
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]
-- length' [1,2,3] = 3
-- length' [] = 0
-- length' "abc" = 3
-}