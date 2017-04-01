module Checksum (validate) where

import Data.List (reverse)

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | n < 10    = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l@(x:xs)
    | length l `mod` 2 == 0 = (x * 2) : doubleEveryOther xs
    | otherwise             = x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum $ concat $ map toDigits xs

validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0

