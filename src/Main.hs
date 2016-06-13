{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)

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

onMessage :: Message -> Bot GameState ()
onMessage message = do
    saveCurrentUserIdToState message
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
onInlineQuery iq = do
    gameId <- setupNewGame iq
    inlineText <- r SendInvitation
    invitationText <- r Invitation
    let inviationMsg = InputTextMessageContent invitationText Nothing Nothing
    yesButton <- inlineKeyboardButton <$> r Yes
    noButton <- inlineKeyboardButton <$> r No
    rulesButton <- inlineKeyboardButton <$> r Rules
    let inlineKeyboard = Just $ InlineKeyboardMarkup [
            [
                yesButton   { ikb_callback_data = Just $ toJson $ InviteResponse gameId True}
              , noButton    { ikb_callback_data = Just $ toJson $ InviteResponse gameId False}
              , rulesButton { ikb_url = Just "http://telegram.me/connect_four_bot" }
            ]
          ]
    let inlineResult = (inlineQueryResultArticle gameId inlineText inviationMsg) {
                           iq_res_reply_markup = inlineKeyboard
                       }
    let response = (answerInlineQueryRequest (query_id iq) [inlineResult]) {
                     query_cache_time = Just 0
        }
    bot answerInlineQuery response
    liftIO $ putStrLn $ show response
    return ()

onCallbackQuery :: CallbackQuery -> Bot GameState ()
onCallbackQuery cq = undefined

saveCurrentUserIdToState :: Message -> Bot GameState ()
saveCurrentUserIdToState message = do
    state <- getState
    putState $ state {currentUserId = getUserIdFromMessage message}

getUserIdFromMessage :: Message -> T.Text
getUserIdFromMessage message =
    maybe "" (T.pack.show.user_id) $ from message

setupNewGame :: InlineQuery -> Bot GameState T.Text
setupNewGame iq = do
    state@(GameState {gamesMap = oldGamesMap}) <- getState
    let newGameId = T.pack.show.Map.size $ oldGamesMap
    putState state{gamesMap = Map.insert newGameId newGame oldGamesMap}
    return newGameId
  where
    player1 = Player (user_id.query_from $ iq) (user_first_name.query_from $ iq)
    newGame = Game newBoard player1 Nothing Nothing

toJson :: ToJSON a => a -> T.Text
toJson = toStrict.decodeUtf8.encode

fromJson :: FromJSON a => T.Text -> Maybe a
fromJson = decode.encodeUtf8.fromStrict

r :: InterfaceText -> Bot GameState T.Text
r text = do
    GameState {currentUserId = userId, languagePrefMap = pref} <- getState
    let lang = Map.findWithDefault DE userId pref
    return $ render lang text

