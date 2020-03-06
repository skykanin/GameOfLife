module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, (!?), iterateN)

data Board =
  Board
    { _board :: Vector Cell
    , size :: Int
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

findIndex :: Int -> Int -> Int -> Int
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

findNeighbours :: Board -> (Int, Int) -> Maybe (Vector Cell)
findNeighbours (Board board w) (x, y) = do
  let index = findIndex w x y
  (cx, cy, _) <- board !? index
  let indecies = map (\(f, g) -> (f cx, g cy)) neighbours
  Nothing

step :: Board -> Board
step = undefined
