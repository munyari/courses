import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import AParser

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "abParser" $ do

    it "parses 'abcdef'" $
        runParser abParser "abcdef"
        `shouldBe` Just (('a','b'),"cdef")

    it "fails to parse 'aebcdf'" $
        runParser abParser "aebcdf"
        `shouldBe` Nothing

    describe "abParser_" $ do

        it "parses 'abcdef'" $
            runParser abParser_ "abcdef"
            `shouldBe` Just ((),"cdef")

        it "fails to parse 'aebcdf'" $
            runParser abParser_ "aebcdf"
            `shouldBe` Nothing

    describe "intPair" $ do

      it "parses an integer pair" $
          runParser intPair "12 34"
          `shouldBe` Just ([12, 34], "")

      it "fails to parse a nonpair" $
          runParser intPair "12 hello"
          `shouldBe` Nothing

    describe "intOrUppercase" $ do

      it "parses integers" $
          runParser intOrUppercase "342abcd"
          `shouldBe` Just ((), "abcd")

      it "parses uppercase letters" $
          runParser intOrUppercase "XYZ"
          `shouldBe` Just ((), "YZ")

      it "fails to parse lowercase" $
          runParser intOrUppercase "foo"
          `shouldBe` Nothing

