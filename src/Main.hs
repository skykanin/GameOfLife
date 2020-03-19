module Main where

import Game (initialGame, step)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Rendering (gameAsPicture)

backgroundColour :: Color
backgroundColour = makeColor 255 255 255 255

window = InWindow "GameOfLife" (640, 500) (10, 10)

main :: IO ()
main =
  simulate window backgroundColour 2 initialGame gameAsPicture (\_ _ -> step)
