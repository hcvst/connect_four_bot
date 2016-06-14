{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map as Map
import Data.Monoid ((<>), mappend)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict, fromStrict)

import Telewrap
import Web.Telegram.API.Bot

import Connect4Game
import I18N
import Types

data GameState = GameState
    {
      gs_gamesMap :: GamesMap
    , gs_currentUserId :: T.Text
    , gs_languagePrefMap :: LanguagePrefMap
    }

main :: IO ((), BotState GameState)
main = runBot =<< newBot token handlers newGameState
  where
    token = "131224483:AAHizcRXIIQzb2sXOwbhGw-nYUuD_BPN7sQ"
    newGameState = GameState Map.empty "" Map.empty
    handlers = MessageHandlers (Just onMessage) (Just onInlineQuery) (Just onCallbackQuery) (Just printUpdate)

printUpdate :: Update -> Bot GameState ()
printUpdate = liftIO.putStrLn.show

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
    yesButton <- inlineKeyboardButton <$> (mappend <$> return "\x2705 " <*> r Yes)
    noButton <- inlineKeyboardButton <$> ((<>) <$> return "\x274C " <*> r No)
    rulesButton <- inlineKeyboardButton <$> ((<>) <$> return "\x2139 " <*> r Rules)
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
onCallbackQuery cq = do
    case fromJson =<< cq_data cq of
        Just ir@(InviteResponse{}) -> handleInviteResponse ir cq
        Just br@(ButtonResponse{}) -> handleButtonResponse br cq
        Nothing -> return ()

handleInviteResponse :: ButtonEvent -> CallbackQuery -> Bot GameState ()
handleInviteResponse InviteResponse{ir_gameId=gameId, ir_accepted=False} cq = do
    state@GameState{gs_gamesMap = gamesMap} <- getState
    case Map.lookup gameId gamesMap of
       Just game ->
           if (playerId.player1 $ game) == (user_id.cq_from $ cq)
           then do
             let notification = "\x274C Sorry. You cannot decline your own invitation."
             bot answerCallbackQuery $ AnswerCallbackQueryRequest (cq_id cq) (Just notification) Nothing
             return ()
           else do
             let player1response = (user_first_name.cq_from $ cq) <> " declined your invitation."
             bot sendMessage $ sendMessageRequest (T.pack.show.playerId.player1 $ game) player1response
             let editMessage = "Ok, maybe another time."
             bot editMessageText $ EditMessageTextRequest Nothing Nothing (cq_inline_message_id cq) editMessage Nothing
                                     Nothing Nothing
             return ()
       Nothing -> return ()

handleInviteResponse InviteResponse{ir_gameId=gameId, ir_accepted=True} cq = do
     state@GameState{gs_gamesMap = gamesMap} <- getState
     case Map.lookup gameId gamesMap of
       Just game ->
           if (playerId.player1 $ game) == (user_id.cq_from $ cq)
           then do
             let notification = "\x274C Sorry. You cannot accept your own invitation."
             bot answerCallbackQuery $ AnswerCallbackQueryRequest (cq_id cq) (Just notification) Nothing
             return ()
           else do
             let player2 = Just $ Player (user_id.cq_from $ cq) (user_first_name.cq_from $ cq) Red
             let updatedGame = game{player2=player2, turnHas=player2}
             putState state{gs_gamesMap = Map.insert gameId updatedGame gamesMap  }
             let player1response = (user_first_name.cq_from $ cq) <> " accepted your invitation."
             bot sendMessage $ sendMessageRequest (T.pack.show.playerId.player1 $ game) player1response
             let player2notification = "Great. You get to go first."
             bot answerCallbackQuery $ AnswerCallbackQueryRequest (cq_id cq) (Just player2notification) Nothing
             updateGameBoard gameId updatedGame (cq_inline_message_id cq)
       Nothing -> return ()

handleButtonResponse :: ButtonEvent -> CallbackQuery -> Bot GameState ()
handleButtonResponse ButtonResponse{br_gameId=gameId, br_button=button} cq = do
     state@GameState{gs_gamesMap = gamesMap} <- getState
     case Map.lookup gameId gamesMap of
       Just game -> case turnHas game of
                      Just Player{playerId=pid, checker=ckr} | pid == (user_id.cq_from $ cq) -> do
                           case dropChecker ckr button (board game) of
                             Left err -> do
                               bot answerCallbackQuery $ AnswerCallbackQueryRequest (cq_id cq) (Just $ T.pack err) Nothing
                               return ()
                             Right newBoard -> do
                               let nextPlayer = if turnHas game == player2 game then Just (player1 game) else (player2 game)
                               let updatedGame = game{board = newBoard, turnHas = nextPlayer}
                               putState state{gs_gamesMap = Map.insert gameId updatedGame gamesMap}
                               updateGameBoard gameId updatedGame (cq_inline_message_id cq)
                      _ -> do
                           let notification = "It is not your turn."
                           bot answerCallbackQuery $ AnswerCallbackQueryRequest (cq_id cq) (Just notification) Nothing
                           return ()
       Nothing -> return ()


updateGameBoard :: T.Text -> Game -> Maybe T.Text -> Bot GameState ()
updateGameBoard gameId game inlineMessageId = do
    case winner.board $ game of
        None -> do
                let editMessage = hasTurnToText game <> "\n\n" <> boardToText (board game) <> columnLabelsText
                let inlineKeyboard = Just $ InlineKeyboardMarkup [[
                      (inlineKeyboardButton (T.pack.show.(+1) $ i)){ ikb_callback_data = Just $ toJson $ ButtonResponse gameId i}
                      | i <- [0..6]
                      ]]
                bot editMessageText $ EditMessageTextRequest Nothing Nothing inlineMessageId editMessage Nothing
                                      Nothing inlineKeyboard
        checker -> do
                let winner = checkerToText checker <> " " <> if checker == Blue
                                                             then name.player1 $ game
                                                             else maybe "Player 2" name (player2 game)
                let editMessage = winner <> " wins!\n\n" <> boardToText (board game)
                bot editMessageText $ EditMessageTextRequest Nothing Nothing inlineMessageId editMessage Nothing
                                      Nothing Nothing
    return ()

saveCurrentUserIdToState :: Message -> Bot GameState ()
saveCurrentUserIdToState message = do
    state <- getState
    putState $ state {gs_currentUserId = getUserIdFromMessage message}

getUserIdFromMessage :: Message -> T.Text
getUserIdFromMessage message =
    maybe "" (T.pack.show.user_id) $ from message

setupNewGame :: InlineQuery -> Bot GameState T.Text
setupNewGame iq = do
    state@(GameState {gs_gamesMap = oldGamesMap}) <- getState
    putState state{gs_gamesMap = Map.insert (newGameId oldGamesMap) newGame oldGamesMap}
    return $ newGameId oldGamesMap
  where
    player1 = Player (user_id.query_from $ iq) (user_first_name.query_from $ iq) Blue
    newGame = Game newBoard player1 Nothing Nothing
    newGameId = T.pack.show.Map.size

hasTurnToText :: Game -> T.Text
hasTurnToText game = maybe "Inconsistent Gamestate" hasTurnText (turnHas game)
  where
    hasTurnText player = checkerToText (checker player) <> " " <> name player <> "'s turn."

boardToText :: Board -> T.Text
boardToText = T.concat.map(`T.append` "\n").map checkersToText
  where
    checkersToText :: [Checker] -> T.Text
    checkersToText = T.concat.map checkerToText

checkerToText :: Checker -> T.Text
checkerToText Red  = "\x1F534"
checkerToText Blue = "\x1F535"
checkerToText None = "\x26AA"

columnLabelsText :: T.Text
columnLabelsText = "\x0031\x20E3\x0032\x20E3\x0033\x20E3\x0034\x20E3\x0035\x20E3\x0036\x20E3\x0037\x20E3"

toJson :: ToJSON a => a -> T.Text
toJson = toStrict.decodeUtf8.encode

fromJson :: FromJSON a => T.Text -> Maybe a
fromJson = decode.encodeUtf8.fromStrict

r :: InterfaceText -> Bot GameState T.Text
r text = do
    GameState {gs_currentUserId = userId, gs_languagePrefMap = pref} <- getState
    let lang = Map.findWithDefault EN userId pref
    return $ render lang text
