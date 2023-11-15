module Exercises where

import Prelude hiding (product)

------------------------------------------------------------------------
--                          Exercise 1.1 (*)                          --
------------------------------------------------------------------------
-- Look at the two ways to calculate `double (double 2)` on pages 3 and 4 in the
-- book Programming in Haskell.  Give another possible calculation for the
-- result of `double (double 2)`.


-- Complete the following block comment.
{-
double (double 2)
= double 2 + double 2
= (2 + 2) + double 2
= (2 + 2) + (2 + 2)
= 4 + (2 + 2)
= 4 + 4
= 8
-}

------------------------------------------------------------------------
--                          Exercise 1.2 (*)                          --
------------------------------------------------------------------------
-- Show that `sum [x] = x` for any number `x`. Use the definition of `sum`
-- stated on page 9 in the book Programming in Haskell.

-- sum [] = 0
-- sum (n:ns) = n + sum ns

-- Complete the following block comment.
{-
sum [x]
= x + sum []
= x + 0
= x
-}

------------------------------------------------------------------------
--                          Exercise 1.3 (*)                          --
------------------------------------------------------------------------
-- Define the function 'product' that produces the product of a list of numbers,
-- and show using your definition that product [2,3,4] == 24.

product :: Num p => [p] -> p
product [] = 1 --base case
product (n:ns) = n * product ns --recursive case

{-
product [2,3,4]
= 2 * product [3,4]
= 2 * 3 * product [4]
= 2 * 3 * 4 * product []
= 2 * 3 * 4 * 1
= 24
-}
