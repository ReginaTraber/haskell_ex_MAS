{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Exercises where

-------------------------------------------------------------------------------
--                             Exercise 4.1 (**)                             --
-------------------------------------------------------------------------------
-- Using library functions, define a function halve :: [a] -> ([a],[a]) that
-- splits an even-lengthed list into two halves. For example:
-- > halve [1,2,3,4,5,6]
-- ([1,2,3],[4,5,6])

{-
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (length xs `div` 2) xs
-}

-------------------------------------------------------------------------------
--                             Exercise 4.2 (**)                             --
-------------------------------------------------------------------------------
-- Define a function third :: [a] -> a that returns the third element in a list that contains at least
-- this many elements using:
-- (a) head and tail;
-- (b) list indexing !!;
-- (c) pattern matching.

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

-- head [1, 2, 3]
-- 1

-- tail [1, 2, 3]
-- [2,3]

thirdB :: [a] -> a
thirdB xs = xs !! 2

-- ['a', 'b', 'c'] !! 0
-- 'a'

thirdC :: [a] -> a
thirdC (_ : _ : x : _) = x

-------------------------------------------------------------------------------
--                             Exercise 4.3 (**)                             --
-------------------------------------------------------------------------------
-- Consider a function safeTail :: [a] -> [a] that behaves in the same way as
-- tail except that it maps the empty list to itself rather than producing an
-- error. Using tail and the function null :: [a] -> Bool that decides if a list
-- is empty or not, define safetail using:

-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) pattern matching.

safeTailA :: [a] -> [a]
safeTailA xs = if null xs then xs else tail xs
-- safeTailA xs = if null xs then [] else tail xs

safeTailB :: [a] -> [a]
safeTailB xs
  | null xs = xs
  | otherwise = tail xs

-- safeTailB xs
--   | null xs = []
--   | otherwise = tail xs

safeTailC :: [a] -> [a]
safeTailC [] = []
safeTailC (_ : xs) = xs

-------------------------------------------------------------------------------
--                             Exercise 4.4 (**)                             --
-------------------------------------------------------------------------------
-- In a similar way to && in section 4.4, show how the disjunction operator ||
-- can be defined in four different ways using pattern matching.

-- -- (a)
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

-- -- (b)
-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True

-- -- (c)
-- (||) :: Bool -> Bool -> Bool
-- True || _ = True
-- False || b = b

-- -- (d)
-- (||) :: Bool -> Bool -> Bool
-- b || c
--   | b == c = b
--   | otherwise = True

-------------------------------------------------------------------------------
--                             Exercise 4.5 (**)                             --
-------------------------------------------------------------------------------
-- Without using any other library functions or operators, show how the meaning
-- of the following pattern matching definition for logical conjunction && can
-- be formalised using conditional expressions:

-- True && True = True
-- _ && _ = False

-- -- (a) Same as 4.6
-- (&&) :: Bool -> Bool -> Bool
-- True && b = b
-- False && _ = False

-- -- -- (b)
-- (&&) :: Bool -> Bool -> Bool
-- a && b =
--   if a
--     then
--       if b
--         then True
--         else False
--     else False

-- -- -- (c)
-- (&&) :: Bool -> Bool -> Bool
-- a && b =
--   if a == True
--     then b
--     else False

-- -- -- (d)
-- (&&) :: Bool -> Bool -> Bool
-- a && b =
--   if a == True
--     then
--       if b == True
--         then True
--         else False
--     else False


-------------------------------------------------------------------------------
--                             Exercise 4.6 (**)                             --
-------------------------------------------------------------------------------
-- Do the same for the following alternative definition, and note the difference
-- in the number of conditional expressions that are required:
-- True && b = b
-- False && _ = False

-- see above

-------------------------------------------------------------------------------
--                             Exercise 4.7 (**)                             --
-------------------------------------------------------------------------------
-- Show how the meaning of the following curried function definition can be
-- formalised in terms of lambda expressions:

-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z

mult :: Int -> (Int -> (Int -> Int))
-- mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-------------------------------------------------------------------------------
--                             Exercise 4.8 (**)                             --
-------------------------------------------------------------------------------
-- The Luhn algorithm is used to check bank card numbers for simple errors such
-- as mistyping a digit, and proceeds as follows:
-- consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
-- if the total is divisible by 10, the card number is valid.
-- Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts
-- 9 if the result is greater than 9. For example:
-- > luhnDouble 3
-- 6
-- > luhnDouble 6
-- 3
-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank
-- card number is valid. For example:
-- > luhn 1 7 8 4
-- True
-- > luhn 4 7 8 3
-- False
-- In the exercises for chapter 7 we will consider a more general version of
-- this function that accepts card numbers of any length.

luhnDouble :: Int -> Int
luhnDouble x
  | x * 2 > 9 = x * 2 - 9
  | otherwise = x * 2

-- luhnDouble x =
--   if x * 2 > 9
--     then x * 2 - 9
--     else x * 2

-- luhnDouble x
--   | x * 2 > 9 = x * 2 - 9
--   | otherwise = x * 2

-- luhnDouble x = (x * 2) `mod` 9

-- luhnDouble x = if x * 2 > 9 then x * 2 - 9 else x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
