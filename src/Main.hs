module Main where

import Game (initialGame, step, testBoard)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Rendering (gameAsPicture)

backgroundColour :: Color
backgroundColour = makeColor 255 255 255 255

window = InWindow "GameOfLife" (640, 500) (10, 10)

main :: IO ()
main =
  simulate window backgroundColour 30 testBoard gameAsPicture (\_ _ -> step)
