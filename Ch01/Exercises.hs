module Exercises where

import Prelude hiding (product)

------------------------------------------------------------------------
--                          Exercise 1.1 (*)                          --
------------------------------------------------------------------------
-- Look at the two ways to calculate `double (double 2)` on pages 3 and 4 in the
-- book Programming in Haskell.  Give another possible calculation for the
-- result of `double (double 2)`.

-- from the book, page 3 and 4

{-
double (double 2)
= { applying the inner double }
double (2 + 2)
= { applying + }
double 4
= { applying double }
4 + 4
= { applying + }
8
-}

{-
double (double 2)
= { applying the outer double }
double 2 + double 2
= { applying the first double }
(2 + 2) + double 2
= { applying the first + }
4 + double 2
= { applying double }
4 + (2 + 2)
= { applying the second + }
4 + 4
= { applying + }
8
-}

-- Solution to the exercise (book solution)

{-
double (double 2)
= { applying the inner double }
double (2 + 2)
= { applying double }
(2 + 2) + (2 + 2)
= { applying the first + }
4 + (2 + 2)
= { applying the second + }
4 + 4
= { applying + }
8
-}

{-
double (double 2)
= { applying the outer double }
(double 2) + (double 2)
= { applying the second double }
(double 2) + (2 + 2)
= { applying the second + }
(double 2) + 4
= { applying double }
(2 + 2) + 4
= { applying the first + }
4 + 4
= { applying + }
8
-}

------------------------------------------------------------------------
--                          Exercise 1.2 (*)                          --
------------------------------------------------------------------------
-- Show that `sum [x] = x` for any number `x`. Use the definition of `sum`
-- stated on page 9 in the book Programming in Haskell.

-- sum :: Num p => [p] -> p --type declaration
-- sum [] = 0 --base case
-- sum (n:ns) = n + sum ns --recursive case

-- Complete the following block comment.
{-
sum [x]
= { applying sum }
x + sum []
= { applying sum }
x + 0
= { applying + }
x
-}

------------------------------------------------------------------------
--                          Exercise 1.3 (*)                          --
------------------------------------------------------------------------
-- Define the function 'product' that produces the product of a list of numbers,
-- and show using your definition that product [2,3,4] == 24.

-- product :: Num p => [p] -> p --type declaration
-- product [] = 1 --base case
-- product (n:ns) = n * product ns --recursive case

{-
product [2,3,4]
= { applying product }
2 * (product [3,4])
= { applying product }
2 * (3 * product [4])
= { applying product }
2 * (3 * (4 * product []))
= { applying product }
2 * (3 * (4 * 1))
= { applying * }
24
-}

------------------------------------------------------------------------
--                          Exercise 1.4 (*)                          --
------------------------------------------------------------------------
-- How should the definition of the function qsort be modified so that it produces a reverse sorted
-- version of a list?

-- original definition

-- qsort :: Ord a => [a] -> [a] --type declaration
-- qsort [] = [] --base case
-- qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger --recursive case
--                where
--                  smaller = [a | a <- xs, a <= x]
--                  larger  = [b | b <- xs, b > x]

-- modified definition
{-
qsort :: Ord a => [a] -> [a] --type declaration
qsort [] = [] --base case
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller --recursive case
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]
-}

------------------------------------------------------------------------
--                          Exercise 1.5 (*)                          --
------------------------------------------------------------------------
-- What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider
-- the example qsort [2,2,3,1,1].

-- ghci> qsort [2,2,3,1,1]
-- [1,2,3]
-- equal elements are removed
