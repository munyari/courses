{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
import Data.Tree
import Control.Arrow (second, first)
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e : l) (empFun e + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1++l2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 >= f2   = gl1
    | otherwise  = gl2

treeFold :: Monoid b => (a -> [b] -> b) -> b -> Tree a -> b
treeFold f elem Node { rootLabel = r, subForest = l } = f r (map (treeFold f elem) l)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp = second (glCons emp) . foldr (<>) (mempty, mempty)
-- right : no boss
-- left : with boss

maxFun :: Tree Employee -> GuestList
maxFun et = uncurry moreFun $ treeFold nextLevel (mempty, mempty) et
