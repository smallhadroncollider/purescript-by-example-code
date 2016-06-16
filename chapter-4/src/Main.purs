module Main where

import Prelude
import Data.Array
import Data.Foldable
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
factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n)(pairs n)
