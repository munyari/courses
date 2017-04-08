import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.List (foldl')
import Fun (fun1, fun1', fun2, fun2')
import FoldTree
import MyFoldl
import Sieve (sieveSundaram)
import Xor
import Map (map')

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "fun1" $ do

    it "fun1' on empty input" $
        fun1' []
        `shouldBe` fun1 []

    it "works on random array" $
        fun1' [31, 34, 21, 53, 31, 24, 20, 54, 45, 47]
        `shouldBe` fun1 [31, 34, 21, 53, 31, 24, 20, 54, 45, 47]


    describe "fun2" $ do
        it "works on 1" $
            fun2' 1
            `shouldBe` fun2 1

        it "works on 78" $
            fun2' 78
            `shouldBe` fun2 78

    describe "foldTree" $ do
        it "works on input \"ABCEFGHIJ\"" $
            foldTree "ABCDEFGHIJ"
            `shouldBe` Node 3
                        (Node 2
                            (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
                            'I'
                            (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf))
                        'J'
                        (Node 2 (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
                            'H'
                            (Node 0 Leaf 'C' Leaf))


    describe "xor" $ do
        it "works for [True, True, True, False]" $
            xor [True, True, True, False]
            `shouldBe` True

        it "works for [True]" $
            xor [True]
            `shouldBe` True

        it "works for [False, False, False]" $
            xor [False, False, False, False]
            `shouldBe` False

    describe "map'" $ do
        it "works like map with predicate" $
            map' (==3) [1..5]
            `shouldBe` map (==3) [1..5]

        it "works like map with multiplication" $
            map' (*3) [1..5]
            `shouldBe` map (*3) [1..5]

    describe "foldl'" $ do
        it "works with subtraction on [4..20]" $
            myFoldl (-) 0 [4..20]
            `shouldBe` foldl' (-) 0 [4..20]

        it "works with subtraction on [3,5,19,6,1]" $
            myFoldl (-) (-9) [3,5,19,6,1]
            `shouldBe` foldl' (-) (-9) [3,5,19,6,1]

    describe "sieve" $ do
        it "works with 25" $
            sieveSundaram 25
            `shouldBe` [3,5,9,11,15,21,23,27,29,31,33,35,37,39,41,43,45,47,49,51]
