module Rendering where

import Board (Board(..))
import Data.Vector (toList)
import Graphics.Gloss

squareSize :: Num a => a
squareSize = 25

startF :: Float -> (Float, Float)
startF n = (-s, s)
  where
    s = 30 * (n / 2)

gameAsPicture :: Board -> Picture
gameAsPicture (Board vec size) =
  Pictures
    [ Color color sq
    | (x, y, b) <- toList vec
    , let xv = f + (x * 30)
          yv = s - (y * 30)
          sq = square xv yv
          color = if b then black else red
    ]
  where
    (f, s) = startF size

square :: Float -> Float -> Picture
square x y = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
  where
    s = squareSize
