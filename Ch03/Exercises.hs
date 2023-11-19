module Exercises where

-------------------------------------------------------------------------------
--                              Exercise 3.1 (*)                             --
-------------------------------------------------------------------------------
-- What are the types of the following values?
-- Uncomment your answers in order for them to be checked by the compiler.

e3_1_1 = ['a','b','c']
e3_1_1 :: [Char]

e3_1_2 = ('a','b','c')
e3_1_2 :: (Char, Char, Char)

e3_1_3 = [(False,'O'),(True,'1')]
e3_1_3 :: [(Bool, Char)]

e3_1_4 = ([False,True],['0','1'])
e3_1_4 :: ([Bool], [Char])

e3_1_5 = [tail, init, reverse]
e3_1_5 :: [[a] -> [a]]


-------------------------------------------------------------------------------
--                              Exercise 3.2 (*)                             --
-------------------------------------------------------------------------------
-- Write down definitions that have the following types; it does not matter what
-- the definitions actually do as long as they are type correct. The type of
-- your defined function may be more general than the types defined below.

bools :: [Bool]
bools = [True]

nums :: [[ Int ]]
nums = [[1]]

add :: Num a => a -> a -> a -> a
add x y z = x + y + z

copy :: b -> (b, b)
copy x = (x,x)

apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x


-------------------------------------------------------------------------------
--                              Exercise 3.3 (**)                            --
-------------------------------------------------------------------------------
-- What are the types of the following functions?
-- Hint: take care to include the necessary class constraints in the types if
-- the functions are defined using overloaded operators.
-- Uncomment your answers in order for them to be checked by the compiler.


second xs = head (tail xs)
-- second :: [a] -> a

swap (x,y) = (y,x)
-- swap :: (b, a) -> (a, b)

pair x y = (x,y)
-- pair :: a -> b -> (a, b)

double x = x*2
--double :: Num a => a -> a

palindrome xs = reverse xs == xs
-- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
--twice :: (t -> t) -> t -> t


-------------------------------------------------------------------------------
--                              Exercise 3.4 (*)                             --
-------------------------------------------------------------------------------
-- Check your answers to the preceding three questions using GHCi.
-- Copy and paste your ghci session into the block comment below.
{-
ghci> second xs = head (tail xs)
ghci> :t second
second :: [a] -> a
ghci> swap (x,y) = (y,x)
ghci> :t swap
swap :: (b, a) -> (a, b)
ghci> pair x y = (x,y)
ghci> :t pair
pair :: a -> b -> (a, b)
ghci> double x = x*2
ghci> :t double
double :: Num a => a -> a
ghci> palindrome xs = reverse xs == xs
ghci> :t palindrome
palindrome :: Eq a => [a] -> Bool
ghci> twice f x = f (f x)
ghci> :t twice
twice :: (t -> t) -> t -> t
-}

-------------------------------------------------------------------------------
--                              Exercise 3.5 (**)                            --
-------------------------------------------------------------------------------
-- Why is it not feasible in general for function types to be instances of the
-- Eq class? When is it feasible? Hint: two functions of the same type are equal
-- if they always return equal results for equal arguments. Try to define a
-- function that computes whether two functions (its inputs) are equal.

-- Type your answer into the block comment below.
{-
In Haskell, functions are not generally instances of the Eq class because comparing functions for equality can be a complex and undecidable problem due to the nature of functions and the halting problem.
The Eq class in Haskell is defined as follows:
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
For a type to be an instance of Eq, you need to provide definitions for the equality (==) and inequality (/=) operators.
However, functions, being potentially infinite and undecidable in their behavior, cannot be directly compared for equality.
If the functions are defined for a finite range of values, it would be possible to test them for equality.
-}
