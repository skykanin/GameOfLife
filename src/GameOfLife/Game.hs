module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, iterateN)

type Board = Vector (Float, Float, Bool)

initialGame :: Board
initialGame = liftA2
  (\x y -> (x, y, False))
  (iterateN 20 (+1) 0)
  (iterateN 20 (\x -> x - 1) 0)

step :: Board -> Board
step = undefined
