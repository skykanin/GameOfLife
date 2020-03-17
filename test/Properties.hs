module Main where

import Game (findIndex, helper)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Index function" $ do
      it "returns the correct index given w, x and y" $ do
        findIndex 20 0 0 `shouldBe` 0
        findIndex 20 3 3 `shouldBe` 3 + 20 * 3
        findIndex 5 3 4 `shouldBe` 3 + 5 * 4
        findIndex 8 15 10 `shouldBe` 15 + 8 * 10
    describe "Index helper function" $ do
      it "returns the same as findIndex for happy path" $ do
        helper 20 0 0 (succ, succ) `shouldBe` Just (findIndex 20 (succ 0) (succ 0))
        helper 10 3 5 (succ, pred) `shouldBe` Just (findIndex 10 (succ 3) (pred 5))
        helper 20 4 4 (pred, succ) `shouldBe` Just (findIndex 20 (pred 4) (succ 4))
        helper 20 1 1 (pred, pred) `shouldBe` Just (findIndex 20 (pred 1) (pred 1))
      it "returns Nothing in case of negative values" $ do
        helper 20 0 0 (succ, pred) `shouldBe` Nothing
        helper 11 0 1 (pred, pred) `shouldBe` Nothing
        helper 30 2 0 (succ, pred) `shouldBe` Nothing
        helper 3 1 (-2) (succ, succ) `shouldBe` Nothing
