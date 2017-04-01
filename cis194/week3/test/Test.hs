import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Hopscotch (skips)
import LocalMaxima (localMaxima)
import Histogram (histogram)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "skips" $ do

    it "works on string 'ABCD'" $
        skips "ABCD"
        `shouldBe` ["ABCD", "BD", "C", "D"]

    it "works on string 'hello!'" $
        skips "hello!"
        `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

    it "works on list [1]" $
        skips [1]
        `shouldBe` ([[1]] :: [[Integer]])

    it "works on list [True,False]" $
        skips [True,False]
        `shouldBe` [[True,False], [False]]

    describe "localMaxima" $ do
        it "works on [2,9,5,6,1]" $
            localMaxima [2,9,5,6,1]
            `shouldBe` [9,6]

        it "works on [2,3,4,1,5]" $
            localMaxima [2,3,4,1,5]
            `shouldBe` [4]


        it "works on [1,2,3,4,5]" $
            localMaxima [1,2,3,4,5]
            `shouldBe` []

    describe "histogram" $ do
        it "works on [1,1,1,5]" $
            histogram [1,1,1,5]
            `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789\n"

        it "works on [1,4,5,4,6,6,3,4,2,4,9]" $
            histogram [1,4,5,4,6,6,3,4,2,4,9]
            `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
