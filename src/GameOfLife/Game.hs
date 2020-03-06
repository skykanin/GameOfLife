module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, (!?), iterateN)

data Board =
  Board
    { _board :: Vector Cell
    , size :: Float
    }
  deriving (Show)

type Cell = (Float, Float, Bool)

boardSize :: Num a => a
boardSize = 20

initialGame :: Board
initialGame =
  Board
    (liftA2
       (\x y -> (x, y, False))
       (iterateN boardSize (+ 1) 0)
       (iterateN boardSize (\n -> n - 1) 0))
    boardSize

findIndex :: Num a => a -> a -> a -> a
findIndex width x y = x + width * y

neighbours :: [(Float -> Float, Float -> Float)]
neighbours =
  [ (succ, id)
  , (succ, succ)
  , (succ, pred)
  , (id, succ)
  , (id, pred)
  , (pred, id)
  , (pred, succ)
  , (pred, pred)
  ]

findNeighbours :: Board -> (Float, Float) -> Maybe (Vector Cell)
findNeighbours (Board board w) (x, y) = do
  let index = findIndex w x y
  (cx, cy, _) <- board !? floor index
  let indecies = map (\(f, g) -> findIndex (f cx) (g cy)) neighbours
  Nothing

step :: Board -> Board
step = undefined
