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