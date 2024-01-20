double :: (Num a) => a -> a
double x = x + x

quadruple :: (Num a) => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- product :: (Num a) => [a] -> a
-- product [] = 1

average :: (Foldable t) => t Int -> Int
average ns = sum ns `div` length ns

-- length :: (Num b) => [a] -> b
-- length [] = 0
-- length (_ : xs) = 1 + length xs

-- div :: (Integral a) => a -> a -> a
-- div x y = x `Prelude.div` y

-- sum :: (Num a) => [a] -> a
-- sum [] = 0
-- sum (n : ns) = n + sum ns