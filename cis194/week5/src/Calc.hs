{-# LANGUAGE FlexibleInstances #-}
module Calc where

import qualified Data.Map as M

import ExprT
import Parser (parseExp)
import StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add l r) = eval l + eval r
eval (ExprT.Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit ExprT.Add ExprT.Mul s of
                Just expr -> Just $ eval expr
                Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a
        | a <= 0    = False
        | otherwise = True
    add a b = a || b
    mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

instance Expr Program where
    lit a = [PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
