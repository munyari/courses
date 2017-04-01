import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Checksum (validate)
import Hanoi (hanoi)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "validate checksum" $ do
        it "correctly validates correct number" $
            validate 4012888888881881
            `shouldBe` True

        it "correctly rejects a wrong number" $
            validate 4012888888881882
            `shouldBe` False

        describe "hanoi" $ do
            it "correctly rearranges pegs" $
                hanoi 2 "a" "b" "c"
                `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
