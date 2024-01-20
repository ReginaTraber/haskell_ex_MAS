module Exercises where

-------------------------------------------------------------------------------
--                              Exercise 3.1 (*)                             --
-------------------------------------------------------------------------------
-- What are the types of the following values?
-- Uncomment your answers in order for them to be checked by the compiler.

e3_1_1 :: [Char]
e3_1_1 = ['a', 'b', 'c']


e3_1_2 :: (Char, Char, Char)
e3_1_2 = ('a', 'b', 'c')


e3_1_3 :: [(Bool, Char)]
e3_1_3 = [(False, 'O'), (True, '1')]


e3_1_4 :: ([Bool], [Char])
e3_1_4 = ([False, True], ['0', '1'])

e3_1_5 :: [[a] -> [a]]
e3_1_5 = [tail, init, reverse]

-- tail :: [a] -> [a]
-- tail (_ : xs) = xs
-- Return all the elements of a list except the first one. The list must be non-empty
-- tail [1, 2, 3] = [2,3]
-- tail [1] = []
-- tail [] = *** Exception: Prelude.tail: empty list

-- init :: [a] -> [a]
-- init [_] = []
-- init (x : xs) = x : init xs
-- Return all the elements of a list except the last one. The list must be non-empty
-- init [1, 2, 3] = [1,2]
-- init [1] = []
-- init [] = *** Exception: Prelude.init: empty list

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x : xs) = reverse xs ++ [x]
-- reverse xs returns the elements of xs in reverse order. xs must be finite


-------------------------------------------------------------------------------
--                              Exercise 3.2 (*)                             --
-------------------------------------------------------------------------------
-- Write down definitions that have the following types; it does not matter what
-- the definitions actually do as long as they are type correct. The type of
-- your defined function may be more general than the types defined below.

-- There are a number of other possible answers for bools, nums and add.

bools :: [Bool]
bools = [True]
-- bools = [False,True]

nums :: [[Int]]
nums = [[1]]
-- nums = [[1,2],[3,4],[5,6]]

add :: (Num a) => a -> a -> a -> a
add x y z = x + y + z

copy :: b -> (b, b)
copy x = (x, x)

apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x

-------------------------------------------------------------------------------
--                              Exercise 3.3 (**)                            --
-------------------------------------------------------------------------------
-- What are the types of the following functions?
-- Hint: take care to include the necessary class constraints in the types if
-- the functions are defined using overloaded operators.
-- Uncomment your answers in order for them to be checked by the compiler.


second :: [a] -> a
second xs = head (tail xs)
-- second (_ : xs) = head xs
-- secons [1,2,3,4] = 2

-- tail :: [a] -> [a]
-- tail (_ : xs) = xs
-- tail [1,2,3,4] = [2,3,4]

-- head :: [a] -> a
-- head (x : _) = x
-- head [2,3,4] = 2

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
-- swap (x, y) = (y, x)
-- swap (1,2) = (2,1)

pair :: a -> b -> (a, b)
pair x y = (x, y)
-- pair x y = (x, y)
-- pair 1 2 = (1,2)

double :: Num a => a -> a
double x = x * 2
-- double x = x * 2
-- double 2 = 4

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
-- palindrome [1,2,3,2,1] = True
-- palindrome [1,2,3,4,5] = False

twice :: (t -> t) -> t -> t
twice f x = f (f x)
-- twice (*2) 2 = 8
-- twice reverse [1,2,3,4] = [1,2,3,4]
-- twice (++ "ab") "test" = "testabab"

-------------------------------------------------------------------------------
--                              Exercise 3.4 (*)                             --
-------------------------------------------------------------------------------
-- Check your answers to the preceding three questions using GHCi.
-- Copy and paste your ghci session into the block comment below.
{-
-- console: ghci
ghci> second xs = head (tail xs)
ghci> :t second --alternative: :type second
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
In Haskell, functions are not generally instances of the Eq class because
comparing functions for equality can be a complex and undecidable problem due to
the nature of functions and the halting problem.
The Eq class in Haskell is defined as follows:
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
For a type to be an instance of Eq, you need to provide definitions for the
equality (==) and inequality (/=) operators. However, functions, being potentially
infinite and undecidable in their behavior, cannot be directly compared for equality.
If the functions are defined for range of values, it would be possible to test them
for equality.
-}