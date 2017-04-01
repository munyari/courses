{-# LANGUAGE TupleSections #-}

module Hopscotch (skips) where
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)

each :: Int -> [a] -> [a]
each m = unfoldr (\x -> fmap (, drop m x) (listToMaybe x))


skips :: [a] -> [[a]]
skips xs = [each start (drop (start-1) xs) | start <- [1..length xs]]
