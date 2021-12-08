module Main where

import qualified Data.Foldable as F

data SubmarineVector = SubmarineVector {
    forward :: Int
    , down :: Int
} deriving (Show)

instance Semigroup SubmarineVector where
    (<>) one two = SubmarineVector (forward one + forward two) (down one + down two)

instance Monoid SubmarineVector where
    mempty = SubmarineVector 0 0

vectFromString :: String -> SubmarineVector
vectFromString s
    | take 2 s == "up" = SubmarineVector 0 (-(read (drop 2 s)))
    | take 7 s == "forward" = SubmarineVector (read (drop 7 s)) 0
    | take 4 s == "down" = SubmarineVector 0 (read (drop 4 s))
    | otherwise = mempty

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let resultant = F.fold (map vectFromString linesOfFile)
    print (forward resultant * down resultant)
     
