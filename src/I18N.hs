{-# LANGUAGE OverloadedStrings #-}

module I18N where

import Data.Monoid ((<>))
import qualified Data.Text as T

data Lang = EN | DE

data InterfaceText =
      Start
    | Invite

render :: Lang -> InterfaceText -> T.Text
render EN = render_en
render DE = render_de


-- English
render_en :: InterfaceText -> T.Text
render_en Start =
       "Please invite a friend to play Connect 4 or visit the Connect 4 Bot group"
    <> " https://telegram.me/Connect4BotGroup to find other players or to ask questions."

render_en Invite =
       "Invite"


-- Deutsch
render_de :: InterfaceText -> T.Text
render_de Start =
       "Bitte lade einen Spieler zu einer Partie Vier Gewinnt ein oder besuche unsere"
    <> " Gruppe https://telegram.me/Connect4BotGroup um Spieler zu finden oder"
    <> " Fragen zu stellen."

render_de Invite =
       "Spieler einladen"

