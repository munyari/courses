module Xor (xor) where

xor :: [Bool] -> Bool
xor = foldr (/=) False
