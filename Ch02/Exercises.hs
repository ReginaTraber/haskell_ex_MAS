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

--test.hs
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
e_2_2_a = (2^3)*4

e_2_2_b :: Int
e_2_2_b = (2*3)+(4*5)

e_2_2_c :: Int
e_2_2_c = 2+(3*(4^5))
