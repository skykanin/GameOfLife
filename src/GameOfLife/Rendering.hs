module Rendering where

import Graphics.Gloss
import Game (Board)

squareSize :: Num a => a
squareSize = 25

start :: (Float, Float)
start = (-640 / 2, 500 / 2 - squareSize)

gameAsPicture :: Board -> Picture
gameAsPicture _ = Pictures
  [ Color red (square x y)
  , Color green (square (x+25) y)
  , Color blue (square x (y - 25))
  ]
    where (x, y) = start

square :: Float -> Float -> Picture
square x y = Polygon [ (x,y) , (x+s, y) , (x+s, y+s), (x, y+s) ]
  where s = squareSize
