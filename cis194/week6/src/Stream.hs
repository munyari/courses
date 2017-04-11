{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Stream where

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a sa) = Stream (f a) $ streamMap f sa

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s $ streamFromSeed f (f s)

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a sa) (Stream b sb) = Stream a $ Stream b $ interleaveStreams sa sb

interleaveStreams' :: Stream a -> Stream a -> Stream a
interleaveStreams' (Stream a sa) b = Stream a $ interleaveStreams' b sa

ruler :: Stream Integer
ruler = interleaveStreams' (streamRepeat 0) (streamMap (+1) ruler)

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger x = Stream x $ streamRepeat 0
    negate = streamMap (*(-1))
    (+) (Stream x xs) (Stream y ys) = Stream (x+y) $ xs + ys
    (*) (Stream x xs) r@(Stream y ys) = Stream (x*y) $ streamMap (*x) ys + (xs * r)

instance Fractional (Stream Integer) where
    (/) a@(Stream x xs) b@(Stream y ys) = Stream (x `div` y) (streamMap (`div` y) (xs - (a/b)*ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) =
        Matrix (a1*a2+b1*c2) (a1*b2+b1*d2) (c1*a2+d1*c2) (c1*b2+d1*d2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case Matrix 1 1 1 0 ^ n of
            (Matrix _ f _ _) -> f
