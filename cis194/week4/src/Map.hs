module Map (map') where

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []
