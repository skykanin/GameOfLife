module Game where

import Data.Vector (Vector, (!?), filter, fromList, length, mapMaybe)
import Prelude hiding (filter, length)

data Board =
  Board
    { _board :: Vector Cell
    , _size :: Float
    }
  deriving (Eq, Show)

type Cell = (Float, Float, Bool)

boardSize :: Num a => a
boardSize = 20

-- game board is zero indexed
initialGame :: Board
initialGame = shipBoard

createBoard :: Float -> Board
createBoard size = Board {_board = fromList board, _size = size}
  where
    board = [(y, x, False) | x <- iterator, y <- iterator]
    iterator = [0 .. pred size]

lineBoard :: Board
lineBoard = b {_board = line <$> _board b}
  where
    b = createBoard 5
    line (1, 2, False) = (1, 2, True)
    line (2, 2, False) = (2, 2, True)
    line (3, 2, False) = (3, 2, True)
    line c = c

shipBoard :: Board
shipBoard = b {_board = ship <$> _board b}
  where
    b = createBoard boardSize
    ship (1, 19, False) = (1, 19, True)
    ship (2, 18, False) = (2, 18, True)
    ship (0, 17, False) = (0, 17, True)
    ship (1, 17, False) = (1, 17, True)
    ship (2, 17, False) = (2, 17, True)
    ship c = c

-- find index for zero indexed board
findIndex :: (Num a, Ord a) => a -> a -> a -> Maybe a
findIndex s x y
  | x >= 0 && y >= 0 && s > x && s > y = Just $ x + s * y
  | otherwise = Nothing

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

neighboursToIndices :: Cell -> Float -> Vector Float
neighboursToIndices (cx, cy, _) w =
  mapMaybe (\(f, g) -> findIndex w (f cx) (g cy)) neighbours

findNeighbours :: Board -> (Float, Float) -> Maybe (Vector Cell)
findNeighbours (Board board w) (x, y) =
  mapMaybe findCell . nti <$> (findCell =<< index)
  where
    index = findIndex w x y
    findCell = (board !?) . floor
    nti = (`neighboursToIndices` w)

-- Game rules
-- Any live cell with two or three live neighbours survives
-- Any dead cell with three live neighbors becomes a live cell.
-- All other live cells die in the next generation. Similarly, all other dead cells stay dead.
alive :: Vector Cell -> Vector Cell
alive = filter (\(_, _, cellState) -> cellState)

applyRules :: Board -> Cell -> Maybe Cell
applyRules board c@(x, y, True) =
  (\v ->
     if length v == 2 || length v == 3
       then c
       else (x, y, False)) .
  alive <$>
  findNeighbours board (x, y)
applyRules board c@(x, y, False) =
  (\v ->
     if length v == 3
       then (x, y, True)
       else c) .
  alive <$>
  findNeighbours board (x, y)

-- If indexing of cells out of bounds return initial game board
step :: Board -> Board
step board@(Board b s) =
  maybe initialGame (`Board` s) (traverse (applyRules board) b)
