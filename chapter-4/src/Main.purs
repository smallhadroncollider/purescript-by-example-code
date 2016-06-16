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
