import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Calc
import Parser
import ExprT (ExprT(..))
-- import StackVM

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "eval" $ do

    it "evaluates (2+3) * 4" $
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
        `shouldBe` 20

    describe "evalStr" $ do
        it "evaluates (2+3)*4" $
            evalStr "(2+3)*4"
            `shouldBe` Just 20

        it "evaluates 2+3*4" $
            evalStr "2+3*4"
            `shouldBe` Just 14

        it "does not evaluate 2+3*" $
            evalStr "2+3*"
            `shouldBe` Nothing

