module Main where

import Game (initialGame)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Rendering (gameAsPicture)

backgroundColour :: Color
backgroundColour = makeColor 255 255 255 255

window = InWindow "GameOfLife" (640,480) (100,100)

main :: IO ()
main = play window backgroundColour 30 initialGame gameAsPicture (const id) (const id)
