module Main where

import Prelude
import Data.Array
import Data.Tuple
import Data.Foldable
import Control.MonadPlus (guard)
import Control.Monad.Eff.Console (infoShow)

main = infoShow (fib 10)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Generates an array of all possible number pairs from 1 to n
pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

-- Tests number pairs for factors 
factors :: Int -> Array (Tuple Int Int)
factors n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    pure $ Tuple i j

isPrime :: Int -> Boolean
-- equivalent to eq ((length (factors n)) 1), using composition
isPrime = eq 1 <<< length <<< factors

-- generate pythagorean triples
pythagoreanTriples :: Int -> Array (Array Int)
pythagoreanTriples n = do
    a <- 1 .. n
    -- from a to n to avoid duplicates (e.g. [3,4,5] & [4,3,5])
    b <- a .. n
    -- from b to n to avoid duplicates
    c <- b .. n
    guard $ a * a + b * b == c * c
    [[a, b, c]]

-- sum a list of integers
sum :: Array Int -> Int
-- equivalent to n = foldl (+) 0 n
-- equivalent to n = foldl (\a b -> a + b) 0 n
sum = foldl (+) 0

-- foldl === reduce
-- joins array of ints
joinInts :: Array Int -> String
joinInts = foldl (\acc n -> acc <> show n) ""

-- reverse joins array of ints
revJoinInts :: Array Int -> String
revJoinInts = foldr (\n acc -> acc <> show n) ""
