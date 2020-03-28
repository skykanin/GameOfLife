module Board where

import Data.Vector

-- The data type representation of the board
data Board =
  Board
    { _board :: Vector Cell
    , _size :: Float
    }
  deriving (Eq, Show)

type Cell = (Float, Float, Bool)

createBoard :: Float -> Board
createBoard size = Board {_board = fromList board, _size = size}
  where
    board = [(y, x, False) | x <- iterator, y <- iterator]
    iterator = [0 .. pred size]
