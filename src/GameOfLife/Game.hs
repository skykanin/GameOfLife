module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, iterateN)

type Board = Vector (Float, Float, Bool)

initialGame :: Board
initialGame = liftA2
  (\x y -> (x, y, False))
  (iterateN 10 (+1) 0)
  (iterateN 10 (\x -> x - 1) 0)
