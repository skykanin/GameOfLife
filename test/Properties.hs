module Main where

import Data.Vector (fromList)
import Game (Board(..), Cell, applyRules, findIndex, findNeighbours, step)
import Test.Hspec

createBoard :: Float -> Board
createBoard size = Board {_board = fromList board, _size = size}
  where
    board = [(y, x, False) | x <- iterator, y <- iterator]
    iterator = [0 .. pred size]

testBoard20 :: Board
testBoard20 = createBoard 20

toSquare :: Cell -> Cell
toSquare (2, 2, False) = (2, 2, True)
toSquare (3, 2, False) = (3, 2, True)
toSquare (2, 3, False) = (2, 3, True)
toSquare (3, 3, False) = (3, 3, True)
toSquare c = c

toLine :: Cell -> Cell
toLine (2, 3, False) = (2, 3, True)
toLine (3, 3, False) = (3, 3, True)
toLine (4, 3, False) = (4, 3, True)
toLine c = c

squareBoardMin :: Board
squareBoardMin = b2 {_board = (\(x, y, _) -> (x, y, True)) <$> _board b2}
  where
    b2 = createBoard 2

squareBoard3 :: Board
squareBoard3 = b3 {_board = sq <$> _board b3}
  where
    b3 = createBoard 3
    sq (0, 0, False) = (0, 0, True)
    sq (1, 0, False) = (1, 0, True)
    sq (0, 1, False) = (0, 1, True)
    sq (1, 1, False) = (1, 1, True)
    sq c = c

squareBoard20 :: Board
squareBoard20 = testBoard20 {_board = fmap toSquare (_board testBoard20)}

lineBoard5 :: Board
lineBoard5 = b5 {_board = ln <$> _board b5}
  where
    b5 = createBoard 5
    ln (1, 2, False) = ln (1, 2, True)
    ln (2, 2, False) = ln (2, 2, True)
    ln (3, 2, False) = ln (3, 2, True)
    ln c = c

resLineBoard5 :: Board
resLineBoard5 = b5 {_board = ln <$> _board b5}
  where
    b5 = createBoard 5
    ln (2, 1, False) = ln (2, 1, True)
    ln (2, 2, False) = ln (2, 2, True)
    ln (2, 3, False) = ln (2, 3, True)
    ln c = c


lineBoard20 :: Board
lineBoard20 = testBoard20 {_board = fmap toLine (_board testBoard20)}

main :: IO ()
main =
  hspec $ do
    describe "Index function" $ do
      it "returns the correct index given a width and coordinates" $ do
        findIndex 20 0 0 `shouldBe` Just 0
        findIndex 20 3 3 `shouldBe` Just (3 + 20 * 3)
        findIndex 5 3 4 `shouldBe` Just (3 + 5 * 4)
        findIndex 11 4 7 `shouldBe` Just (4 + 11 * 7)
      it "returns Nothing in case of negative values" $ do
        findIndex 3 0 (-2) `shouldBe` Nothing
        findIndex 8 (-1) 4 `shouldBe` Nothing
        findIndex 5 (-5) (-2) `shouldBe` Nothing
        findIndex 5 1 (-7) `shouldBe` Nothing
      it
        "returns Nothing in case of coords being equal or greater than board size" $ do
        findIndex 5 5 4 `shouldBe` Nothing
        findIndex 7 10 7 `shouldBe` Nothing
        findIndex 8 15 10 `shouldBe` Nothing
        findIndex 20 20 3 `shouldBe` Nothing
    describe "Find neighbour cells function" $ do
      it "returns all eight neighbours when cell is not against a wall" $ do
        length <$> findNeighbours (createBoard 3) (1, 1) `shouldBe` Just 8
        length <$> findNeighbours (createBoard 10) (5, 5) `shouldBe` Just 8
        length <$> findNeighbours (createBoard 20) (18, 18) `shouldBe` Just 8
        length <$> findNeighbours (createBoard 15) (13, 13) `shouldBe` Just 8
      it "returns five neighbors when cell is against wall, but not in a corner" $ do
        length <$> findNeighbours (createBoard 3) (0, 1) `shouldBe` Just 5
        length <$> findNeighbours (createBoard 3) (2, 1) `shouldBe` Just 5
        length <$> findNeighbours (createBoard 3) (1, 2) `shouldBe` Just 5
        length <$> findNeighbours (createBoard 3) (1, 0) `shouldBe` Just 5
      it "returns three neighbors when cell is in a corner" $ do
        length <$> findNeighbours (createBoard 3) (0, 0) `shouldBe` Just 3
        length <$> findNeighbours (createBoard 3) (2, 0) `shouldBe` Just 3
        length <$> findNeighbours (createBoard 3) (0, 2) `shouldBe` Just 3
        length <$> findNeighbours (createBoard 3) (2, 2) `shouldBe` Just 3
    describe "Applies game logic rules correctly" $ do
      it "remains the same shape if board has square shape" $ do
        step squareBoardMin `shouldBe` squareBoardMin
        step squareBoard3 `shouldBe` squareBoard3
        step squareBoard20 `shouldBe` squareBoard20
      it "oscilates between 2 states if board has line shape" $ do
        step lineBoard5 `shouldBe` resLineBoard5
        step resLineBoard5 `shouldBe` lineBoard5
        (step . step) lineBoard5 `shouldBe` lineBoard5
