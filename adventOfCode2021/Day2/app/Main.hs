module Main where

import Day2
import Data.Foldable ( Foldable(fold) )


main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let part1Vectors = map read linesOfFile
    print (part1 part1Vectors)
    let instructions = map read linesOfFile
    print (part2 instructions)