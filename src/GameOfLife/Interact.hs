{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Interact where

import Board (Board(..))
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.Either (isLeft)
import Data.Vector (Vector, (!?), iterateN, length, zipWith)
import GHC.Generics
import Prelude hiding (length, readFile, zipWith)
import System.IO (readLn)

newtype PlainBoards =
  PlainBoards
    { _getBoards :: Vector PlainBoard
    }
  deriving (Generic, Show)

instance FromJSON PlainBoards

data PlainBoard =
  PlainBoard
    { _pb :: Vector Float
    , _ps :: Float
    }
  deriving (Generic, Show)

type Boards = Vector Board

instance FromJSON PlainBoard

filename :: FilePath
filename = "initialGame.json"

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight b Nothing = Left b
maybeToRight _ (Just a) = Right a

intToBool :: Float -> Bool
intToBool 1 = True
intToBool _ = False

fromPlain :: PlainBoard -> Board
fromPlain (PlainBoard pb ps) = Board {_board = board, _size = ps}
  where
    coords = do
      x <- iterateN (floor ps) (+ 1) 0
      y <- iterateN (floor ps) (+ 1) 0
      pure (y, x)
    board = zipWith (\(y, x) i -> (y, x, intToBool i)) coords pb

decodeBoard :: ByteString -> Maybe (Vector PlainBoard)
decodeBoard file = _getBoards <$> decode file

getBoards :: IO (Either String Boards)
getBoards = do
  file <- readFile $ "resources/" <> filename
  let res = (fromPlain <$>) <$> decodeBoard file
  pure $ maybeToRight "Couldn't decode json file" res

getBoard :: Either String Boards -> Int -> Either String Board
getBoard boards idx =
  boards >>= maybeToRight "Vector index out of bounds" . (!? idx)

readIdxToBoard :: IO (Either String Board)
readIdxToBoard = do
  boards <- getBoards
  let res idx = pure $ getBoard boards idx
  if isLeft boards
    then res 0
    else do
      either print (print . format . length) boards
      arg <- readLn :: IO Int
      res arg
  where
    format len = "Select a board by index (0 - " <> show (pred len) <> ")"
