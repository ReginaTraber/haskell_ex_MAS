module Exercises where

-------------------------------------------------------------------------------
--                              Exercise 2.1 (*)                             --
-------------------------------------------------------------------------------
-- Work through all the examples of chapter "First Steps" from page 14 to 20 in
-- the book Programming in Haskell using GHCi.  Copy and paste your ghci session
-- and the contents of the file `test.hs` you created into the block comment
-- below.

{-
ghci> 2+3*4
14
ghci> (2+3)*4
20
ghci> sqrt (3^2 + 4^2)
5.0
ghci> head [1,2,3,4,5]
1
ghci> tail [1,2,3,4,5]
[2,3,4,5]
ghci> [1,2,3,4,5] !! 2
3
ghci> take 3 [1,2,3,4,5]
[1,2,3]
ghci> drop 3 [1,2,3,4,5]
[4,5]
ghci> length [1,2,3,4,5]
5
ghci> sum [1,2,3,4,5]
15
ghci> product [1,2,3,4,5]
120
ghci> [1,2,3] ++ [4,5]
[1,2,3,4,5]
ghci> reverse [1,2,3,4,5]
[5,4,3,2,1]
-}

-- test.hs
{-
double x = x +x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns
-}

{-
ghci Ch02/test.hs
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( Ch02/test.hs, interpreted )
Ok, one module loaded.
ghci> quadruple 10
40
ghci> take (double 2) [1,2,3,4,5]
[1,2,3,4]
ghci> :r
[1 of 2] Compiling Main             ( Ch02/test.hs, interpreted ) [Source file changed]
Ok, one module loaded.
ghci> factorial 10
3628800
ghci> average [1,2,3,4,5]
3
-}

-------------------------------------------------------------------------------
--                              Exercise 2.2 (*)                             --
-------------------------------------------------------------------------------
-- Parenthesise the following numeric expressions:

-- 2^3*4 2*3+4*5 2+3*4^5

e_2_2_a :: Int
e_2_2_a = (2 ^ 3) * 4

e_2_2_b :: Int
e_2_2_b = (2 * 3) + (4 * 5)

e_2_2_c :: Int
e_2_2_c = 2 + (3 * (4 ^ 5))

-------------------------------------------------------------------------------
--                              Exercise 2.3 (*)                             --
-------------------------------------------------------------------------------
-- The script below contains three syntactic errors. Correct these errors and then
-- check that your script works properly using GHCi.

computeN :: Int
computeN = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-------------------------------------------------------------------------------
--                              Exercise 2.4 (*)                             --
-------------------------------------------------------------------------------
-- The library function 'last' selects the last element of a non-empty list; for example,
-- last [1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library
-- functions introduced in this chapter. Can you think of another possible definition?

{-
last :: [a] -> a
last xs = head (drop (length xs - 1) xs)
last xs = head (reverse xs)
last xs = xs !! (length xs - 1)
-}

-- ['a', 'b', 'c'] !! 2
-- 'c'

-- head [1, 2, 3]
-- 1

-- reverse [2,5,7]
-- [7,5,2]

-- length ['a', 'b', 'c']
-- 3

-- drop 3 [1,2,3,4,5]
-- [4,5]

-------------------------------------------------------------------------------
--                              Exercise 2.5 (*)                             --
-------------------------------------------------------------------------------
-- The library function 'init' removes the last element from a non-empty list; for example,
-- init [1,2,3,4,5] = [1,2,3,4]. Show how 'init' could similarly be defined in two different ways.

{-
init :: [a] -> [a]
init xs = take (length xs - 1) xs
init xs = reverse (tail (reverse xs))
-}

-- take 5 "Hello World!"
-- "Hello"

-- length ['a', 'b', 'c']
-- 3

-- tail [1, 2, 3]
-- [2,3]

-- reverse [2,5,7]
-- [7,5,2]
