module Main where

import Test.Hspec
import Day2
import Data.Foldable ( Foldable(fold) )
import Day2 (SubmarineVector(SubmarineVector), part2Resultant)

exampleLines :: [String]
exampleLines = [
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
    ]

exampleInputVectors :: [SubmarineVector]
exampleInputVectors =  map read exampleLines

exampleInputInstructions :: [SubmarineInstruction]
exampleInputInstructions =  map read exampleLines

main :: IO ()
main = hspec $ do
    describe "Reading a vector" $ do
        it "can parse a forward line" $ do
            read "forward 56" `shouldBe` SubmarineVector 56 0
        it "can parse an up line" $ do
            read "up 13" `shouldBe` SubmarineVector 0 (-13)
        it "can parse a down line" $ do
            read "down 4657" `shouldBe` SubmarineVector 0 4657
        it "uses the fallback if parsing fails" $ do
            read "djhjh 56" `shouldBe` SubmarineVector 0 0

    describe "Reading an instruction" $ do
        it "can parse a forward line" $ do
            read "forward 56" `shouldBe` Drive 56
        it "can parse an up line" $ do
            read "up 13" `shouldBe` Steer (-13)
        it "can parse a down line" $ do
            read "down 4657" `shouldBe` Steer 4657
        it "uses the fallback if parsing fails" $ do
            read "djhjh 56" `shouldBe` Drive 0

    describe "Part 1" $ do
        it "the example input can be folded to the correct resultant vector" $ do
            fold exampleInputVectors `shouldBe` SubmarineVector 15 10
        it "returns the correct value from the provided example" $ do
            part1 exampleInputVectors `shouldBe` 150

    describe "Part 2" $ do
        it "returns 0 if the input list is empty" $ do
            part2 [] `shouldBe` 0
        it "works for a single drive instruction" $ do
            part2 [Drive 10] `shouldBe` 0
        it "returns the correct value from the provided example" $ do
            part2 exampleInputInstructions `shouldBe` 900

    describe "part2Resultant" $ do
        it "works for a single drive instruction" $ do
            part2Resultant [Drive 11] `shouldBe` SubmarineVector 11 0
        it "works for a single steer instruction" $ do
            part2Resultant [Steer 17] `shouldBe` SubmarineVector 0 0
        it "works for a steer then drive" $ do
            part2Resultant [Steer 3, Drive 7] `shouldBe` SubmarineVector 7 21
        it "returns the correct value from the provided example" $ do
            part2Resultant exampleInputInstructions `shouldBe` SubmarineVector 15 60