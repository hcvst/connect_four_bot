{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics

import Connect4Game
import I18N

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

type LanguagePrefMap = Map.Map T.Text Lang

data ButtonEvent = InviteResponse {ir_gameId :: T.Text, ir_accepted :: Bool}
                 | ButtonResponse {br_gameId :: T.Text, br_button :: Int }
                 deriving (Show, Generic)

instance FromJSON ButtonEvent
instance ToJSON ButtonEvent