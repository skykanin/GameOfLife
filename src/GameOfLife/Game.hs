module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, (!?), fromList, iterateN, mapMaybe)

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

neighbours :: Vector (Float -> Float, Float -> Float)
neighbours =
  fromList
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
findNeighbours (Board board w) (x, y) =
  mapMaybe findCell . neighboursToIndices <$> findCell index
  where
    index = findIndex w x y
    findCell = (board !?) . floor
    neighboursToIndices (cx, cy, _) =
      (\(f, g) -> findIndex w (f cx) (g cy)) <$> neighbours

step :: Board -> Board
step b = b
