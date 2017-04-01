-- CIS 194 Homework 3

module Histogram (histogram) where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

draw :: Int -> Map.Map Int Int -> String
draw maxValue myMap = join [greater x y | x <- [maxValue,maxValue-1..1], y <- [0..9]]
    where join l = unlines $ chunksOf 10 $ concat l ++ "==========0123456789"
          greater xval y
            | getVal y myMap >= xval = "*"
            | otherwise = " "

histogram :: [Int] -> String
histogram xs = draw maxValue myMap
    where myMap = foldr (\x y -> Map.insert x (getVal x y + 1) y) Map.empty xs
          maxValue = Map.foldr max (-1) myMap


getVal :: Int -> Map.Map Int Int -> Int
getVal k m = fromMaybe 0 (Map.lookup k m)
