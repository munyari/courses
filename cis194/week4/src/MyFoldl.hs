module MyFoldl (myFoldl) where

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (flip f)
