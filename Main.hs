module Main where

import Conway
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

cellRadius = 2

renderCell :: (Integer, Integer) -> Picture
renderCell (x, y) = Translate (xf*cellRadius*2) (yf*cellRadius*2) (Circle cellRadius)
    where xf = fromIntegral x :: Float
          yf = fromIntegral y :: Float

merge :: [Picture] -> Picture -> Picture
merge [] cur = cur
merge (x:xs) cur = merge xs (x <> cur)

render :: [(Integer, Integer)] -> Picture
render x = merge (map renderCell x) Blank

stepSimulation :: ViewPort -> Float -> [(Integer, Integer)] -> [(Integer, Integer)]
stepSimulation _ _ state = step state

window = InWindow "Conway" (1000, 1000) (10, 10) 

initialState = gosperGliderGun


main :: IO ()
main = simulate window white 10 initialState render stepSimulation