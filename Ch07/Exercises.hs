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
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use and" #-}
module Exercises where

import Prelude hiding (and, any, curry, dropWhile, filter, foldl, foldr, id, length, map, or, product, reverse, sum, takeWhile, uncurry, (.))


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
curry f = \x y -> f (x, y)

-- f(x,y) is element of type c
-- Mathematically : given a function f : X × Y → Z currying transforms it into a function f' : X → (Y → Z)


uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y
-- Mathematically : given a function f : X → (Y → Z) uncurrying transforms it into a function f' : X × Y → Z

-- g:: (Int, Int) -> Int
-- g (x,y) = x + y

-- h :: Int -> Int -> Int
-- h x y = x + y

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


------------------------------------------------------------------------
------------------------------------------------------------------------
--                          Book                                      --
------------------------------------------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
--                          7.1 Basic concepts                        --
------------------------------------------------------------------------

---- twice: takes a function and a value, and returns the result of
---- applying the function twice to the value.

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- twice (*2) 3 = 12
-- twice reverse [1,2,3] = [1,2,3]

------------------------------------------------------------------------
--                          7.2 Processing lists                      --
------------------------------------------------------------------------

-- map: applies a function to every element of a list, producing a new list
-- map (map (+1)) [[1,2,3],[4,5]] = [[2,3,4],[5,6]]
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- recursion
{-
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs
-}
-- map (+1) [1,3,5,7] = [2,4,6,8]
-- map even [1,2,3,4] = [False,True,False,True]
-- map reverse ["abc","def","ghi"] = ["cba","fed","ihg"]
{-
map (map (+1)) [[1,2,3],[4,5]]
= { applying the outer map }
[map (+1) [1,2,3], map (+1) [4,5]]
= { applying the inner maps }
[[2,3,4],[5,6]]
-}

-- filter: selects every element from a list that satisfies a predicate
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

-- recursion
{-
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
-}
-- filter even [1..10] = [2,4,6,8,10]
-- filter (>5) [1..10] = [6,7,8,9,10]
-- filter (/=' ') "abc def ghi" = "abcdefghi"

-- all: decides if every element of a list satisfies a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

-- all even [2,4,6,8] = True

-- any: decides if any element of a list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

-- any odd [2,4,6,8] = False

-- takeWhile: selects elements from a list while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- takeWhile even [2,4,6,7,8] = [2,4,6]

-- dropWhile: removes elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

-- dropWhile even [2,4,6,7,8] = [7,8]

-- sumsqreven: sums the squares of all the even numbers from a list
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^ 2) (filter even ns))

-- sumsqreven [1,2,3,4,5] = 20
-- sumsqreven [] = 0

{-
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
x ^ n = x * x ^ (n - 1)
-}

------------------------------------------------------------------------
--                          7.5 The composition operator              --
------------------------------------------------------------------------

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- idenitity function: id . f = f and f . id = f
id :: a -> a
id = \x -> x

-- composition is associative: (f . g) . h = f . (g . h)

-- compose: composition of a list of functions -- compose [f,g,h,i] = f . g . h . i
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

{-
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

id :: a -> a
id = \x -> x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x : xs) = f x (foldr f v xs)
-}

-- odd' n = not (even n)
odd' :: Int -> Bool
odd' = not . even

-- twice' f x = f (f x)
twice' :: (a -> a) -> a -> a
twice' f = f . f

-- sumsqreven' ns = sum (map (^ 2) (filter even ns))
sumsqreven' :: [Int] -> Int
sumsqreven' = sum . map (^ 2) . filter even

------------------------------------------------------------------------
-- foldr -- fold right
------------------------------------------------------------------------

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x : xs) = f x (foldr f v xs)

-- summing a list of numbers
-- applying foldr (+) 0 to the list [1,2,3,4]
-- foldr (+) 0 [1,2,3,4]
-- foldr (+) 0 (1 : (2 : (3 : (4 : [])))) = 1 + (2 + (3 + (4 + 0)))
-- foldr f v [x1,x2...xn] = x1 `f` (x2 `f` ... (xn `f` v)...)

-- -- sum: summing a list of numbers
-- sum :: (Num a) => [a] -> a
-- sum [] = 0
-- sum (x : xs) = x + sum xs

-- sum' :: (Num a) => [a] -> a
-- sum' = foldr (+) 0

-- -- product: multiplying a list of numbers
-- product :: (Num a) => [a] -> a
-- product [] = 1
-- product (x : xs) = x * product xs

-- product' :: (Num a) => [a] -> a
-- product' = foldr (*) 1

-- -- or: combining a list of boolean values using the logical disjunction operator ||
-- or :: [Bool] -> Bool
-- or [] = False
-- or (x : xs) = x || or xs

-- or' :: [Bool] -> Bool
-- or' = foldr (||) False

-- -- and: combining a list of boolean values using the logical conjunction operator &&
-- and :: [Bool] -> Bool
-- and [] = True
-- and (x : xs) = x && and xs

-- and' :: [Bool] -> Bool
-- and' = foldr (&&) True

-- -- length: computing the length of a list
-- length :: [a] -> Int
-- length [] = 0
-- length (_ : xs) = 1 + length xs

-- length' :: [a] -> Int
-- length' = foldr (\_ n -> 1 + n) 0

-- -- reverse: reversing a list
-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x : xs) = reverse xs ++ [x]

-- -- reverse [] = []
-- -- reverse (x : xs) = snoc x (reverse xs)

-- -- snoc: appending an element to the end of a list
-- -- snoc :: a -> [a] -> [a]
-- -- snoc x xs = xs ++ [x]
-- -- snoc [1,2,3] 4 = [1,2,3,4]

-- reverse' :: [a] -> [a]
-- reverse' = foldr (\x xs -> xs ++ [x]) []

-- -- reverse' :: [a] -> [a]
-- -- reverse' = foldr snoc []

------------------------------------------------------------------------
-- foldl -- fold left
------------------------------------------------------------------------

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x : xs) = foldl f (f v x) xs

-- summing a list of numbers
-- applying foldl (+) 0 to the list [1,2,3,4]
-- foldl (+) 0 [1,2,3,4] = (((0 + 1) + 2) + 3) + 4

-- sum: summing a list of numbers
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- product: multiplying a list of numbers
product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- or: combining a list of boolean values using the logical disjunction operator ||
or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

or' :: [Bool] -> Bool
or' = foldl (||) False

-- and: combining a list of boolean values using the logical conjunction operator &&
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

and' :: [Bool] -> Bool
and' = foldl (&&) True

-- length: computing the length of a list
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

length' :: [a] -> Int
length' = foldl (\n _ -> 1 + n) 0

-- reverse: reversing a list
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- reverse [] = []
-- reverse (x : xs) = snoc x (reverse xs)

-- snoc: appending an element to the end of a list
-- snoc :: a -> [a] -> [a]
-- snoc x xs = xs ++ [x]
-- snoc [1,2,3] 4 = [1,2,3,4]

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []

-- reverse' :: [a] -> [a]
-- reverse' = foldl snoc []
