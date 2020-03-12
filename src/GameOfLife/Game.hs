module Game where

import Control.Applicative (liftA2)
import Data.Vector (Vector, (!?), fromList, iterateN, length, mapMaybe)
import Prelude hiding (length)

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

testBoard :: Board
testBoard = Board {_board = fromList board, size = 20}
  where
    board = [shapeMapper (x, y, False) | x <- [1 .. 20], y <- [1 .. 20]]

shapeMapper :: Cell -> Cell
shapeMapper (2, 3, False) = (2, 3, True)
shapeMapper (3, 3, False) = (3, 3, True)
shapeMapper (4, 3, False) = (4, 3, True)
shapeMapper c = c

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

-- Game rules
-- Any live cell with two or three neighbors survives
-- Any dead cell with three live neighbors becomes a live cell.
-- All other live cells die in the next generation. Similarly, all other dead cells stay dead.
applyRules :: Board -> Cell -> Maybe Cell
applyRules board c@(x, y, True) =
  (\v ->
     if length v == 2 || length v == 3
       then c
       else (x, y, False)) <$>
  findNeighbours board (x, y)
applyRules board c@(x, y, False) =
  (\v ->
     if length v == 3
       then (x, y, True)
       else c) <$>
  findNeighbours board (x, y)

-- If indexing of cells out of bounds return initial game board
step :: Board -> Board
step board@(Board b s) =
  maybe initialGame (`Board` s) (traverse (applyRules board) b)
