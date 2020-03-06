module Main where

import Game (initialGame, step)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Rendering (gameAsPicture)

backgroundColour :: Color
backgroundColour = makeColor 255 255 255 255

window = InWindow "GameOfLife" (640, 500) (100, 100)

main :: IO ()
main =
  simulate window backgroundColour 30 initialGame gameAsPicture (\_ _ -> step)
