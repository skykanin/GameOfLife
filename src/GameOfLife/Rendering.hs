module Rendering where

import Graphics.Gloss
import Game (Board)
import Data.Vector (toList)

squareSize :: Num a => a
squareSize = 25

start :: (Float, Float)
start = (-640 / 2, 500 / 2 - squareSize)

gameAsPicture :: Board -> Picture
gameAsPicture vec = Pictures
  [ if b
    then Color black sq
    else Color red sq
  | (x, y, b) <- toList vec
  , let xv = fst start + (x * 30)
        yv = snd start + (y * 30)
        sq = square xv yv
  ]

square :: Float -> Float -> Picture
square x y = Polygon [ (x,y) , (x+s, y) , (x+s, y+s), (x, y+s) ]
  where s = squareSize
