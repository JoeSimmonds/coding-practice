module Main where

import HelloWorld (greet)
import HaskellSay (haskellSay)

main :: IO ()
main = haskellSay (greet "Haskell")