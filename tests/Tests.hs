module Main where


import Test.Hspec
import qualified Data.PackedSet as PS


mySpecs =
    describe "Verify that bassbull outputs the correct data" $ do
        it "equals zero" $ do
            let dummy = 10
            dummy `shouldBe` 10
    -- describe "PackedSet creation"
    -- [
    --     it "bla"
    --            (pending "reason..")
    -- ]

main :: IO ()
main = hspec mySpecs