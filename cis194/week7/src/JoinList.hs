{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, InstanceSigs #-}
module JoinList where

import Sized
import Data.Monoid ((<>), Monoid(..))
import Scrabble
import Data.Maybe (listToMaybe)
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2

listSize :: (Sized b, Monoid b) => JoinList b a -> Int
listSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ v)
        | i == 0    = Just v
        | otherwise = Nothing
indexJ i jl@(Append _ l r)
        | i >= listSize jl = Nothing
        | i >= listSize l  = indexJ (i-listSize l) r
        | otherwise        = indexJ i l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl@(Append _ l r)
        | i >= listSize jl = Empty
        | i >= leftSize    = dropJ (i-leftSize) r
        | otherwise        = dropJ i l +++ r
        where leftSize = listSize l
dropJ i jl
        | i <= 0    = jl
        | otherwise = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl@(Append _ l r)
        | i >= listSize jl = jl
        | i >= leftSize    = l +++ takeJ (i-leftSize) r
        | otherwise        = takeJ i l
        where leftSize = listSize l
takeJ i jl
        | i <= 0    = Empty
        | otherwise = jl

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ l r) = toString l ++ toString r

    fromString = foldr ((+++) . (\s -> Single (scoreString s, 1) s)) Empty . lines

    line = indexJ

    replaceLine n s b = takeJ n b +++ fromString s +++ dropJ (n+1) b

    numLines = getSize . size . snd . tag

    value = getScore . fst . tag
        where getScore (Score n) = n
