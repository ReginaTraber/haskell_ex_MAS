{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Eta reduce" #-}
module Exercises where

import Prelude hiding (any, curry, dropWhile, map, takeWhile, uncurry)


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
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
--all p = foldr (&&) True . map p

-- (b) Decide if any element of a list satisfies a predicate:
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
--any p = foldr (||) False . map p

-- (c) Select elements from a list while they satisfy a predicate:
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x -> if p x then (x:) else const []) []
--takeWhile _ [] = []
--takeWhile p (x:xs)
--    | p x = x : takeWhile p xs
--    | otherwise = []
-- Rmk: not the same as takeWhile p = filter p

-- (d) Remove elements from a list while they satisfy a predicate:
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
--                          Exercise 7.4 (**)                         --
------------------------------------------------------------------------
-- Using foldl, define a function dec2int :: [Int] -> Int that converts a
-- decimal number into an integer. For example:
-- > dec2int [2,3,4,5]
-- 2345
-- Hint: first write a function that maps a single digit into a single-digit
-- integer.

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0



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


------------------------------------------------------------------------
--                          Exercise 7.6 (**)                         --
------------------------------------------------------------------------
-- A higher-order function unfold that encapsulates a simple pattern of
-- recursion for producing a list can be defined as follows:
-- unfold p h t x | p x = []
--                | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate
-- p is true of the argument, and otherwise produces a non-empty list by applying
-- the function h to this argument to give the head, and the function t to generate
-- another argument that is recursively processed in the same way to produce the tail
-- of the list. For example, the function int2bin can be rewritten more compactly using
-- unfold as follows:
-- int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
-- Redefine the functions chop8, map f and iterate f using unfold.

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) id f

------------------------------------------------------------------------
--                          Exercise 7.7 (**)                         --
------------------------------------------------------------------------
-- Modify the binary string transmitter example to detect simple transmission
-- errors using the concept of parity bits. That is, each eight-bit binary number
-- produced during encoding is extended with a parity bit, set to one if the number
-- contains an odd number of ones, and to zero otherwise. In turn, each resulting
-- nine-bit binary number consumed during decoding is checked to ensure that its
-- parity bit is correct, with the parity bit being discarded if this is the case,
-- and a parity error being reported otherwise.
-- Hint: the library function error :: String -> a displays the given string as an
-- error message and terminates the program; the polymorphic result type ensures
-- that error can be used in any context.

-- not implemented

------------------------------------------------------------------------
--                          Exercise 7.8 (**)                         --
------------------------------------------------------------------------
-- Test your new string transmitter program from the previous exercise using a
-- faulty communication channel that forgets the first bit, which can be modelled
-- using the tail function on lists of bits.

-- not implemented

------------------------------------------------------------------------
--                          Exercise 7.9 (**)                         --
------------------------------------------------------------------------
-- Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that
-- alternately applies its two argument functions to successive elements in a
-- list, in turn about order. For example:
-- > altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

------------------------------------------------------------------------
--                          Exercise 7.10 (**)                        --
------------------------------------------------------------------------
-- Using altMap, define a function luhn :: [Int] -> Bool that implements
-- the Luhn algorithm from the exercises in chapter 4 for bank card numbers
-- of any length. Test your new function using your own bank card.

-- altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- altMap _ _ [] = []
-- altMap f g (x : xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap id (uncurry (+) . (`divMod` 10) . (* 2)) . reverse
