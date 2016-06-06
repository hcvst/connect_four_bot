{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map as Map
import qualified Data.Text as T

import Telewrap
import Web.Telegram.API.Bot

import Connect4Game
import I18N
import Types

data GameState = GameState {
      gamesMap :: GamesMap
    , currentUserId :: T.Text
    , languagePrefMap :: LanguagePrefMap}

main :: IO ((), BotState GameState)
main = runBot =<< newBot token handlers newGameState
  where
    token = "131224483:AAHizcRXIIQzb2sXOwbhGw-nYUuD_BPN7sQ"
    newGameState = GameState Map.empty "" Map.empty
    handlers = MessageHandlers (Just onMessage)
                               (Just onInlineQuery)
                               (Just onCallbackQuery)
                               Nothing

onUpdate :: Update -> Bot GameState ()
onUpdate update = do
    liftIO $ putStrLn.unpack.encodePretty $ update

onMessage :: Message -> Bot GameState ()
onMessage message = do
    saveUserIdToState message
    response <- sendMessageRequest (getChatIdFromMessage message) <$> r Start
    keyboardLabel <- r Invite
    let keyboard = Just $ inlineKeyboardMarkup [
            [(inlineKeyboardButton keyboardLabel ) { ikb_switch_inline_query = Just "" }]
          ]
    bot sendMessage response {
          message_disable_web_page_preview = Just True
        , message_reply_markup = keyboard
    }
    return ()

onInlineQuery :: InlineQuery -> Bot GameState ()
onInlineQuery iq = undefined

onCallbackQuery :: CallbackQuery -> Bot GameState ()
onCallbackQuery cq = undefined

saveUserIdToState :: Message -> Bot GameState ()
saveUserIdToState message = do
    state <- getState
    putState $ state {currentUserId = getUserIdFromMessage message}

getUserIdFromMessage :: Message -> T.Text
getUserIdFromMessage message =
    maybe "" (T.pack.show.user_id) $ from message

r :: InterfaceText -> Bot GameState T.Text
r text = do
    GameState {currentUserId = userId, languagePrefMap = pref} <- getState
    let lang = Map.findWithDefault DE userId pref
    return $ render lang text