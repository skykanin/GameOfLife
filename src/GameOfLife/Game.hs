module Game where

import Data.Vector (Vector, replicate)
import Prelude hiding (replicate)

type Board = Vector (Vector Bool)

initialGame :: Board
initialGame = replicate 10 (replicate 10 False)
