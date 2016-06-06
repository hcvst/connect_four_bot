module Connect4Game (
         newBoard
       , dropChecker
       , winner
       , Checker(..)
       , Board
) where

import Data.List

data Checker = Red | Blue | None deriving (Eq, Show, Ord)

type Row = [Checker]
type Board = [Row]
type Slot = Int
type CheckerInserted = Bool

newBoard :: Board
newBoard = replicate 6 $ replicate 7 None

dropChecker :: Checker -> Slot -> Board -> Either String Board
dropChecker checker slot board =
    if slot `notElem` [0..6] then Left "Not a valid slot"
    else case mapAccumR update False board of
      (False, _) -> Left "Column full"
      (True, updatedBoard) -> Right updatedBoard
  where
    update :: CheckerInserted -> Row -> (CheckerInserted, Row)
    update alreadyInserted row = if alreadyInserted then (alreadyInserted, row) else updateRow row

    updateRow :: Row -> (CheckerInserted, Row)
    updateRow row = case row !! slot of
      None -> (True, insertChecker row)
      _    -> (False, row)

    insertChecker :: Row -> Row
    insertChecker row = take slot row ++ checker : drop (slot+1) row

winner :: Board -> Checker
winner = head.sort.sequence [rowWinner, colWinner, diagonalDownWinner, diagonalUpWinner]

rowWinner :: Board -> Checker
rowWinner = head.head.sort.filter((>=4).length).concatMap group

colWinner :: Board -> Checker
colWinner = rowWinner.transpose

diagonalDownWinner :: Board -> Checker
diagonalDownWinner = colWinner.zipWith (++) (tails $ replicate 5 None)

diagonalUpWinner :: Board -> Checker
diagonalUpWinner = colWinner.zipWith (++) (reverse.tails $ replicate 5 None)
