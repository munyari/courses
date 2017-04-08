module Sieve (sieveSundaram) where

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map (\x -> x*2+1) [1..x] \\ map (\(i, j) -> i + j + 2 * i * j) ij
    where ij = filter (\(i, j) -> i + j + 2 * i * j <= x) cartProd
          cartProd = [(i, j) | j <- [1..x], i <- [1..j]]
