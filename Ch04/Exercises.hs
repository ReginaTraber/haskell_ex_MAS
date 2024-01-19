{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use splitAt" #-}
module Exercises where

import Data.List.NonEmpty (cons)
import Prelude hiding (splitAt, head, tail)

-------------------------------------------------------------------------------
--                             Exercise 4.1 (**)                             --
-------------------------------------------------------------------------------
-- Using library functions, define a function halve :: [a] -> ([a],[a]) that
-- splits an even-lengthed list into two halves. For example:
-- > halve [1,2,3,4,5,6]
-- ([1,2,3],[4,5,6])

{-
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)
-- splitAt 3 [1,2,3,4,5,6] = ([1,2,3],[4,5,6])
-- splitAt 0 [1,2,3] = ([],[1,2,3])
-- splitAt 3 [] = ([],[])

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs
-- take 3 [1,2,3,4,5,6] = [1,2,3]
-- take 0 [1,2] = []
-- take 3 [] = []
-}

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (length xs `div` 2) xs

halve'' :: [a] -> ([a], [a])
halve'' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where
    n = length xs

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

{-
tail :: [a] -> [a]
tail (_ : xs) = xs
-- tail [1, 2, 3] = [2,3]
-- tail [] = *** Exception: Prelude.tail: empty list

-----alternative
tail :: [a] -> [a]
tail (x : xs) = xs
-}

{-
head :: [a] -> a
head (x : _) = x
-- head [1, 2, 3]
-- head [] = *** Exception: Prelude.head: empty list
-}

thirdB :: [a] -> a
thirdB xs = xs !! 2

{-
(!!) :: [a] -> Int -> a
(!!) (x : _) 0 = x
(!!) (_ : xs) n = xs !! (n - 1)
-- ['a', 'b', 'c'] !! 0 = 'a'
-- ['a', 'b', 'c'] !! 2 = 'c'
-- ['a', 'b', 'c'] !! 3 = *** Exception: Prelude.!!: index too large
-- ['a', 'b', 'c'] !! (-1) = *** Exception: Prelude.!!: negative index
-- [] !! 2 = *** Exception: Prelude.!!: index too large
-}

thirdC :: [a] -> a
thirdC (_ : _ : x : _) = x
-- thirdC (a:b:x:d) = x
-- thirdC [1, 2, 3, 4, 5] = 3


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

{-
null :: [a] -> Bool
null [] = True
null (_ : _) = False
-}

{-
tail :: [a] -> [a]
tail (_ : xs) = xs
-- tail [1, 2, 3] = [2,3]
-- tail [] = *** Exception: Prelude.tail: empty list

-----alternative
tail :: [a] -> [a]
tail (x : xs) = xs
-}

safeTailA :: [a] -> [a]
safeTailA xs =
  if null xs
    then xs
    else tail xs

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

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                             Lecture                                       --
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- give back if a integer is even
even :: (Integral a) => a -> Bool
even n = n `mod` 2 == 0

-- even 2 = True

-- split a list at the nth element:
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- splitAt 3 [1,2,3,4,5] = ([1,2,3],[4,5])

-- Reciprocation (multiplicative inverse of a number)
recip :: (Fractional a) => a -> a
recip n = 1 / n

-- recip 2 = 0.5

-------------------------------------------------------------------------------
--                             Conditional expressions                        --
-------------------------------------------------------------------------------

-- Absolute value
abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- abs (-1) = 1
-- abs 1 = 1

-- Signum
-- nested Conditional expressions
signum :: Int -> Int
signum n =
  if n < 0
    then -1
    else
      if n == 0
        then 0
        else 1

-- signum (-6) = -1
-- signum 0 = 0
-- signum 6 = 1

-------------------------------------------------------------------------------
--                             Guarded equations                             --
-------------------------------------------------------------------------------

-- Absolute value
abs' :: Int -> Int
abs' n
  | n >= 0 = n
  | otherwise = -n

-- abs' (-5) = 5
-- abs' 1 = 1

-- Signum
signum' :: Int -> Int
signum' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

-- signum' (-6) = -1
-- signum' 0 = 0
-- signum' 6 = 1

-------------------------------------------------------------------------------
--                             Pattern matching                              --
-------------------------------------------------------------------------------

---- not
not :: Bool -> Bool
not False = True
not True = False

---- &&
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False

-- the following simplified alternatives are more efficient because of lazy evaluation
{-
-- alternative 1
True && True = True
_ && _ = False

-- alternative 2
True && b = b
False && _ = False

-- alternative 3
b && b = b
_ && _ = False

-- alternative 4 with guards
b && c
  | b == c = b
  | otherwise = False
-}

-------------------------------------------------------------------------------
--                             Tuple patterns                                --
-------------------------------------------------------------------------------

-- fst selects the first component of a pair
fst :: (a, b) -> a
fst (x, _) = x

-- fst (1,2) = 1

-- snd selects the second component of a pair
snd :: (a, b) -> b
snd (_, y) = y

-- snd (1,2) = 2

-------------------------------------------------------------------------------
--                             List patterns                                 --
-------------------------------------------------------------------------------

--- test tests if a list contains three characters and starts with 'a'
test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

-- test "abc" = True
-- test "abcd" = False
-- test "ab" = False

-- using cons operator
test' :: [Char] -> Bool
test' ('a' : _) = True
test' _ = False

---- head selects the first element of a non-empty list
head :: [a] -> a
head (x : _) = x

-- head [1,2,3] = 1

---- tail selects all but the first element of a non-empty list
tail :: [a] -> [a]
tail (_ : xs) = xs

-- tail [1,2,3] = [2,3]

---- decomposing lists
-- [1,2,3] is just an abbreviation for 1:(2:(3:[])).
-- cons operator (:) is right-associative
-- [1,2,3] = 1 : (2 : (3 : [])) = 1 : 2 : 3 : []

{-
[1,2,3]
= { list notation }
1 : [2,3]
= { list notation }
1 : (2 : [3])
= { list notation }
1 : (2 : (3 : []))
-}

-------------------------------------------------------------------------------
--                             Lambda expressions                            --
-------------------------------------------------------------------------------

---- add two numbers ----
-- console: ghci
-- ghci> (\x -> x + x) 2
-- 4

add :: Int -> Int -> Int
add x y = x + y

-- add 2 3 = 5

-- lambda expression
add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

---- takes two arguments and returns a list containing only the first argument ---
const :: a -> b -> [a]
const x _ = [x]

-- const [1,2,3] 4 = [[1,2,3]]
-- const (1,2) 3 = [(1,2)]

-- lambda expression
const' :: a -> b -> [a]
const' = \x -> (\_ -> [x])

---- returns the first n odd integers in list ---
odds :: Int -> [Int]
odds n = map f [0 .. n - 1]
  where
    f x = x * 2 + 1

-- lambda expression
odds' :: Int -> [Int]
odds' n = map (\x -> x * 2 + 1) [0 .. n - 1]

-------------------------------------------------------------------------------
--                             Operator sections                             --
-------------------------------------------------------------------------------

{-
---- (+) is the addition function \x -> (\y -> x+y)
(+) :: Int -> Int -> Int
(+) = \x -> (\y -> x + y)
-- (+) 2 3 = 5

---- (1+) is the successor function \y -> 1+y
(1+) :: Int -> Int
(1+) = \y -> 1 + y
-- (1+) 2 = 3

---- (1/) is the reciprocation function \y -> 1/y
(1/) :: (Fractional a) => a -> a
(1/) = \y -> 1 / y
-- (1/) 2 = 0.5

---- (* 2) is the doubling function \x -> x * 2
(*2) :: Int -> Int
(*2) = \x -> x * 2
-- (* 2) 2 = 4

---- (/ 2) is the halving function \x -> x / 2
(/2) :: (Fractional a) => a -> a
(/2) = \x -> x / 2
-- halvingFunction 2 = 1.0

---- sum is the sum function \xs -> foldl (+) 0 xs
-- sum that calculates the sum of a list of integers
sum :: [Int] -> Int
sum = foldl (+) 0
sum' :: [Int] -> Int
sum' = \xs -> foldl (+) 0 xs
-- sum [1,2,3] = 6

--
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x : xs) = foldl f (f v x) xs
-- foldl (+) 1 [1,2,3] =

-}
