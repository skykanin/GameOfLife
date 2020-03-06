module Rendering where

import Data.Vector (toList)
import Game (Board(..))
import Graphics.Gloss

squareSize :: Num a => a
squareSize = 25

start :: (Float, Float)
start = (-640 / 2, 500 / 2 - squareSize)

gameAsPicture :: Board -> Picture
gameAsPicture (Board vec _) =
  Pictures
    [ if b
      then Color black sq
      else Color red sq
    | (x, y, b) <- toList vec
    , let xv = fst start + (x * 30)
          yv = snd start + (y * 30)
          sq = square xv yv
    ]

square :: Float -> Float -> Picture
square x y = Polygon [(x, y), (x + s, y), (x + s, y + s), (x, y + s)]
  where
    s = squareSize
