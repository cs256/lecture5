--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Lecture 5: Recursion I                                                     --
--------------------------------------------------------------------------------

module Lecture5 where

import Prelude hiding ( and, replicate, product, take, length,
                        (++), reverse, concat, splitAt, zip)

--------------------------------------------------------------------------------
-- Factorial

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

--------------------------------------------------------------------------------
-- Fibonacci

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--------------------------------------------------------------------------------
-- Product

product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

--------------------------------------------------------------------------------
-- Demo

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

take :: Int -> [a] -> [a]
take 0 xs     = []
take n []     = []
take n (x:xs) = x : take (n-1) xs

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n-1) x

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

splitAt :: Int -> [a] -> ([a],[a])
--splitAt n xs = (take n xs, drop n xs)
splitAt 0 xs     = ([], xs)
splitAt n []     = ([], [])
splitAt n (x:xs) = (x:ys, zs)
    where
        (ys, zs) = splitAt (n-1) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

--------------------------------------------------------------------------------
