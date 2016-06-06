module Types where

import qualified Data.Map as Map
import qualified Data.Text as T

import Connect4Game

data Player = Player {
      playerId :: Int
    , name :: T.Text
} deriving (Eq, Show)

data Game = Game {
      board :: Board
    , player1 :: Player
    , player2 :: Maybe Player
    , turnHas :: Maybe Player
} deriving (Eq, Show)

type GamesMap = Map.Map T.Text Game