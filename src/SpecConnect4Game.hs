#!/usr/bin/runhaskell

import Control.Monad (foldM, liftM)
import Data.Either

import Test.Hspec

import Connect4Game

main :: IO ()
main = hspec $ do
  describe "A new game board" $ do
    it "has 6 rows" $ do
      length newBoard `shouldBe` 6

    it "has 7 fields in each row" $ do
      (all (==7) $ map length newBoard) `shouldBe` True

    it "has no player checkers" $ do
      (all (==None) $ concat newBoard) `shouldBe` True

  describe "Dropping a checker in a slot" $ do
    it "should change the board" $ do
      Right newBoard `shouldNotBe` dropChecker Red 0 newBoard

    it "should add the player's checker to the board" $ do
      let Right updatedBoard = dropChecker Red 2 newBoard
      (filter (==Red ) $ concat updatedBoard) `shouldBe` [Red]

    it "does not affect the board's width" $ do
      let Right updatedBoard = dropChecker Red 2 newBoard
      (all (==7) $ map length updatedBoard) `shouldBe` True

    it "does not affect the board's height" $ do
      length `liftM` dropChecker Red 3 newBoard `shouldBe` Right 6

    it "changes the bottom row for empty boards first" $ do
      let slot = 5
      let Right updatedBoard = dropChecker Blue slot newBoard
      reverse updatedBoard !! 0 !! slot `shouldBe` Blue

    it "piles checkers on top of previously dropped ones" $ do
      let slot = 5
      let Right updatedBoard = dropChecker Blue slot newBoard >>= dropChecker Red slot
      reverse updatedBoard !! 1 !! slot `shouldBe` Red

    it "must not exceed the bounds of the board" $ do
      dropChecker Blue (-1) newBoard `shouldBe` Left "Not a valid slot"
      dropChecker Blue 7 newBoard `shouldBe` Left "Not a valid slot"

    it "must not overflow the column" $ do
      let Right fullColumnBoard = foldM (\board (slot, checker) -> dropChecker checker slot board) newBoard $
            replicate 6 (0, Blue)
      dropChecker Blue 0 fullColumnBoard `shouldBe` Left "Column full"

  describe "A winning condition" $ do
    it "is never met by a new board" $ do
      winner newBoard `shouldBe` None

    it "is to have four adjacent checkers of the same colour in one row" $ do
      let Right winningBoard = foldM (\board slot -> dropChecker Blue slot board) newBoard [2..5]
      winner winningBoard `shouldBe` Blue

    it "is to have more than four adjacent checkers of the same colour in one row" $ do
      let Right winningBoard = foldM (\board slot -> dropChecker Blue slot board) newBoard [2..6]
      winner winningBoard `shouldBe` Blue

    it "is not met by four or more non-adjacent checkers of the same colour in one row" $ do
      let Right winningBoard = foldM (\board slot -> dropChecker Blue slot board) newBoard [0,1,2,5,6]
      winner winningBoard `shouldBe` None

    it "is to have four adjacent checkers of the same colour in one column" $ do
      let Right winningBoard = foldM (\board slot -> dropChecker Red slot board) newBoard [2,2,2,2]
      winner winningBoard `shouldBe` Red

    it "is to have four adjacent checkers of the same colour diagonally up" $ do
      let Right winningBoard = foldM (\board (slot, checker) -> dropChecker checker slot board) newBoard
            [ (0, Blue)
            , (1, Red), (1, Blue)
            , (2, Red), (2, Red), (2, Blue)
            , (3, Red), (3, Red), (3, Red), (3, Blue)
            ]
      winner winningBoard `shouldBe` Blue

    it "is to have four adjacent checkers of the same colour diagonally down" $ do
      let Right winningBoard = foldM (\board (slot, checker) -> dropChecker checker slot board) newBoard
            [ (0, Blue), (0, Blue), (0, Blue), (0, Red)
            , (1, Blue), (1, Blue), (1, Red)
            , (2, Blue), (2, Red)
            , (3, Red)
            ]
      winner winningBoard `shouldBe` Red
