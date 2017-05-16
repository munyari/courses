import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SExpr
import AParser
import Data.Char (isUpper)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "zeroOrMore" $ do

    it "parses consecutive uppercase letters" $
        runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
        `shouldBe` Just ("ABC","dEfgH")

    it "returns empty string when the parser fails" $
        runParser (zeroOrMore (satisfy isUpper)) "abcdefg"
        `shouldBe` Just ("","abcdefg")

    describe "oneOrMore" $ do
        it "parses consecutive uppercase letters" $
            runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
            `shouldBe` Just ("ABC","dEfgH")

        it "fails when the parser fails" $
            runParser (oneOrMore (satisfy isUpper)) "abcdefg"
            `shouldBe` Nothing

    describe "spaces" $ do
        it "correctly parses spaces" $
            runParser spaces "    ABCdEfgH"
            `shouldBe` Just ("    ", "ABCdEfgH")

        it "does not parse non-spaces" $
            runParser spaces "AB   CdefgH"
            `shouldBe` Just ("",  "AB   CdefgH")

    describe "ident" $ do
        it "parses all alpha identifier" $
            runParser ident "foobar baz"
            `shouldBe` Just ("foobar", " baz")

        it "parses alphanumeric idenitifer" $
            runParser ident "foo33fA"
            `shouldBe` Just ("foo33fA",  "")

        it "fails to parse word beginning with number" $
            runParser ident "2bad"
            `shouldBe` Nothing

        it "fails to parse empty string" $
            runParser ident ""
            `shouldBe` Nothing

    describe "parseSExpr" $ do
        it "parses numeric atoms" $
            runParser parseSExpr "5"
            `shouldBe` Just (A (N 5), "")

        it "parses alphanumeric atoms" $
            runParser parseSExpr "foo3"
            `shouldBe` Just (A (I "foo3"), "")

        it "parses multiple expressions" $
            runParser parseSExpr "(bar (foo) 3 5 874)"
            `shouldBe` Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")

        it "parses nested expressions" $
            runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
            `shouldBe` Just (Comb [Comb [Comb [A (I "lambda"), A (I "x"),
                             Comb [A (I "lambda"), A (I "y"),
                             Comb [A (I "plus"), A (I "x"), A (I "y")]]],
                             A (N 3)], A (N 5)], "")

        it "parses expressions with lots of spaces" $
            runParser parseSExpr "(    lots    of (    spaces    in  )  this ( one ) )"
            `shouldBe` Just (Comb [A (I "lots"), A (I "of"),
                             Comb [A (I "spaces"), A (I "in")],
                             A (I "this"), Comb [A (I "one")]], "")
