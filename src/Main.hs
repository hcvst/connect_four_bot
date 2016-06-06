{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text as T

import Telewrap
import Web.Telegram.API.Bot

import Connect4Game
import Types

data GameState = GameState {gamesMap :: GamesMap}

main :: IO ((), BotState GameState)
main = runBot =<< newBot token handlers newGameState
  where
    token = "131224483:AAHizcRXIIQzb2sXOwbhGw-nYUuD_BPN7sQ"
    handlers = MessageHandlers (Just onMessage) Nothing Nothing
    newGameState = GameState Map.empty

onMessage :: Message -> Bot GameState ()
onMessage = sendMessageResponse "Hello World"

onInlineQuery :: InlineQuery -> Bot GameState ()
onInlineQuery query = undefined

onCallbackQuery :: CallbackQuery -> Bot GameState ()
onCallbackQuery query = undefined