module Day2 (SubmarineVector(SubmarineVector), SubmarineInstruction(Drive, Steer), part1, part2, part2Resultant) where

import Data.Foldable ( Foldable(fold) )
import Control.Monad.State ( MonadState(state), evalState, State )
import Data.Eq (Eq)

data SubmarineInstruction = Drive {distance :: Int} | Steer {amount :: Int} deriving (Show, Eq)

instance Read SubmarineInstruction where
    readsPrec p = \s -> [(buildfromInputString Drive Steer (Drive 0) s, "")]

data SubmarineVector = SubmarineVector {
    forward :: Int
    , down :: Int
} deriving (Show, Eq)

instance Semigroup SubmarineVector where
    (<>) one two = SubmarineVector (forward one + forward two) (down one + down two)

instance Monoid SubmarineVector where
    mempty = SubmarineVector 0 0

buildfromInputString :: (Int -> a) -> (Int -> a) -> a -> String -> a
buildfromInputString horzF vertF fallback s
    | take 2 s == "up" = vertF  (-(read (drop 2 s)))
    | take 7 s == "forward" = horzF (read (drop 7 s))
    | take 4 s == "down" = vertF (read (drop 4 s))
    | otherwise = fallback

instance Read SubmarineVector where
    readsPrec p = \s -> [(buildfromInputString
                                (`SubmarineVector` 0)
                                (SubmarineVector 0)
                                (SubmarineVector 0 0)
                                s
                            , "")]


part1 :: [SubmarineVector] -> Int
part1 xs = forward resultant * down resultant where resultant = fold xs

applyInstruction :: SubmarineInstruction -> Int -> (SubmarineVector, Int)
applyInstruction (Drive x) a = (SubmarineVector x (x * a), a)
applyInstruction (Steer x) a = (SubmarineVector 0 0, a + x)

baz :: [SubmarineInstruction] -> Int -> [SubmarineVector]
baz [] aim = []
baz (x:xs) aim = let (vect, newAim) = applyInstruction x aim in vect : baz xs newAim

part2Resultant :: [SubmarineInstruction] -> SubmarineVector
part2Resultant xs = fold (baz xs 0)

part2 :: [SubmarineInstruction] -> Int
part2 xs = forward resultant * down resultant where resultant = part2Resultant xs
