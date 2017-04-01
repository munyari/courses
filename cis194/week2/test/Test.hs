import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import LogAnalysis 
import Log

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "parseMessage" $ do

    it "parses a well formatted error message" $
        parseMessage "E 2 562 help help"
        `shouldBe` LogMessage (Error 2) 562 "help help"

    it "parses a well formatted error message" $
        parseMessage "I 29 la la la"
        `shouldBe` LogMessage Info 29 "la la la"

    it "parses an unknown message correctly" $
        parseMessage "This is not the right format"
        `shouldBe` Unknown "This is not the right format"

    describe "whatWentWrong" $ do
        it "only captures severe errors" $
            whatWentWrong [LogMessage Info 6 "Completed armadillo processing",
                          LogMessage Info 1 "Nothing to report",
                          LogMessage (Error 99) 10 "Flange failed!",
                          LogMessage Info 4 "Everything normal",
                          LogMessage Info 11 "Initiating self-destruct sequence",
                          LogMessage (Error 70) 3 "Way too many pickles",
                          LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
                          LogMessage Warning 5 "Flange is due for checkup",
                          LogMessage Info 7 "Out for lunch, back in two time steps",
                          LogMessage (Error 20) 2 "Too many pickles",
                          LogMessage Info 9 "Back from lunch"]
            `shouldBe` ["Way too many pickles",
                       "Bad pickle-flange interaction detected",
                       "Flange failed!"]
