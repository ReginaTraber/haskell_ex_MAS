{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use all" #-}
module Exercises where

import Prelude hiding (any, takeWhile, dropWhile, {- map, filter, -} curry, uncurry)


------------------------------------------------------------------------
--                          Exercise 7.1 (*)                          --
------------------------------------------------------------------------
-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions map and filter.

e_7_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
e_7_1 f p xs = map f (filter p xs)


------------------------------------------------------------------------
--                          Exercise 7.2 (**)                         --
------------------------------------------------------------------------
-- Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists. Try to use the
-- higher-order functions and, or, (.), foldr and map within your definitions if
-- possible.
-- Note: in the prelude the first two of these functions are generic functions
-- rather than being specific to the type of lists.

-- (a) Decide if all elements of a list satisfy a predicate:
--     all :: (a -> Bool) -> [a] -> Bool

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
--all p = foldr (&&) True . map p


-- (b) Decide if any element of a list satisfies a predicate:
--     any :: (a -> Bool) -> [a] -> Bool

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
--any p = foldr (||) False . map p


-- (c) Select elements from a list while they satisfy a predicate:
--     takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x -> if p x then (x:) else const []) []
--takeWhile _ [] = []
--takeWhile p (x:xs)
--    | p x = x : takeWhile p xs
--    | otherwise = []
-- Rmk: not the same as takeWhile p = filter p


-- (d) Remove elements from a list while they satisfy a predicate:
--     dropWhile :: (a -> Bool) -> [a] -> [a]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
    | p x = dropWhile p xs
    | otherwise = x:xs
-- Rmk: didn't find a way to use foldr
-- Rmk: not the same as dropWhile p = filter (not . p)


------------------------------------------------------------------------
--                          Exercise 7.3 (**)                         --
------------------------------------------------------------------------
-- Redefine the functions map and filter using foldr.
-- Note: to avoid naming collisions, we'll name these implementations 'myMap'
-- instead of 'map' and 'myFilter' instead of 'filter'.

myMap :: (a -> b) -> [a] -> [b]
--myMap f = foldr (\x xs -> f x : xs) []
--myMap f = foldr ((:) . f) []
--myMap f = foldr (\x -> (:) (f x)) []
myMap f = foldr ((++) . (\x -> [f x])) []



myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter p = foldr (\x xs -> if p x then x:xs else xs) []
--myFilter p = foldr ((++) . (\x -> if p x then [x] else [])) []
--myFilter p = foldr (\x -> if p x then (x:) else id) []
myFilter p = foldr (\x -> (++) (if p x then [x] else [])) []



------------------------------------------------------------------------
--                          Exercise 7.5 (**)                         --
------------------------------------------------------------------------

-- Without looking at the definitions from the standard prelude, define the
-- higher-order library function curry that converts a function on pairs into a
-- curried function, and, conversely, the function uncurry that converts a
-- curried function with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)


uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y


{-
  `curry` and `uncurry` functions provide a concise way to convert between curried and uncurried forms of functions.

  Use `curry` to partially apply arguments, creating specialized versions of functions, and for elegant function composition.

  f :: Int -> Int -> Int
  g :: Int -> Int
  h :: Int

  -- curry example
  f' :: Int -> (Int -> Int)
  g' :: Int -> Int
  h' :: Int

  f' = curry f
  g' = f' 5  -- g' is a partially applied version of f'
  h' = g' 10 -- h' is the result of applying g' to another argument

  Use `uncurry` to adapt functions for compatibility with libraries or situations where an uncurried form is more convenient.

  -- uncurry example
  f :: Int -> Int -> Int
  g :: (Int, Int) -> Int

  -- Convert curried function to uncurried
  g = uncurry f

  result = g (5, 10) -- Using the uncurried function with a tuple
-}



