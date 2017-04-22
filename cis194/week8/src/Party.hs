{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
import Data.Tree
import Control.Arrow (first)
import System.IO
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

treeFold :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = r, subForest = l } = f r (map (treeFold f) l)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp = first (glCons emp)
                . foldr (\(inclBoss, exclBoss) (a1, a2) -> (a1 <> exclBoss, a2 <> moreFun inclBoss exclBoss))
                (mempty, mempty)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

fmtGL :: GuestList -> String
fmtGL (GL l f) = "Total fun: " ++ show f ++ "\n" ++ employeeNames
    where employeeNames = unlines $ map (show . empName) l


main :: IO ()
main = do
    handle <- openFile "company.txt" ReadMode
    contents <- hGetContents handle
    putStr $ fmtGL $ maxFun $ read contents
    hClose handle
