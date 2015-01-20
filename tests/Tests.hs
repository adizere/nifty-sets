module Main where


import Test.Hspec
import qualified Data.PackedSet as PS


mySpecs :: Spec
mySpecs =
    describe "Data.PackedSet construction" $ do
        let p = PS.empty :: PS.PackedSet Int
        it "constructs an empty packet set" $ do
            PS.null p `shouldBe` True
        it "tests the size of an empty set" $ do
            PS.size p `shouldBe` 0
        it "tests the `member` function on an empty set" $ do
            PS.member 0 p `shouldBe` False
        it "tests the `notMember` function on an empty set" $ do
            PS.notMember 0 p `shouldBe` True

        let s = PS.singleton 888 :: PS.PackedSet Int
        it "constructs a singleton set" $ do
            PS.null s `shouldBe` False
            PS.size s `shouldBe` 1
            PS.member 0 s `shouldBe` False
            PS.member 888 s `shouldBe` True
            PS.notMember 0 s `shouldBe` True
            PS.notMember 888 s `shouldBe` False


main :: IO ()
main = hspec mySpecs