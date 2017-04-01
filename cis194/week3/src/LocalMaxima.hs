module LocalMaxima (localMaxima) where

localMaxima :: [Int] -> [Int]
localMaxima xs = map second $ filter secondIsMax  $ zip3 xs (drop 1 xs) (drop 2 xs)
    where second (_,b,_) = b
          first (a,_,_) = a
          third (_,_,c) = c
          secondIsMax x = second x > first x && second x > third x

