module Main where

import Data.Vector (fromList)
import Game (Board(..), findNeighbours, findIndex)
import Test.Hspec

createBoard :: Float -> Board
createBoard size = Board {_board = fromList board, size = size}
  where
    board = [(y, x, False) | x <- iterator, y <- iterator]
    iterator = [0 .. pred size]
    
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
      it "returns Nothing in case of coords being equal or greater than board size" $ do
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
