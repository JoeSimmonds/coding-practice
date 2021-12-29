module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import HelloWorld (greet)

main :: IO ()
main = hspec $ do
    describe "greet" $ do
        it "should prepend hello" $ do
            greet "Nigel" `shouldBe` "Hello Nigel!"

