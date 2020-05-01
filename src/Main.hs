module Main where

import Board (Board)
import Game (step)
import Graphics.Gloss
import Graphics.Gloss.Data.Color()
import Interact (readIdxToBoard)
import Rendering (gameAsPicture)

backgroundColour :: Color
backgroundColour = makeColor 255 255 255 255

window :: Display
window = InWindow "GameOfLife" (640, 500) (10, 10)

run :: Board -> IO ()
run board =
  simulate window backgroundColour 2 board gameAsPicture (\_ _ -> step)

main :: IO ()
main = do
  board <- readIdxToBoard
  either print run board
