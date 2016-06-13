{-# LANGUAGE OverloadedStrings #-}

module I18N where

import Data.Monoid ((<>))
import qualified Data.Text as T

data Lang = EN | DE

data InterfaceText =
      Start
    | Invite
    | SendInvitation
    | Invitation
    | Yes
    | No
    | Rules

render :: Lang -> InterfaceText -> T.Text
render EN = render_en
render DE = render_de

-- English -------------------------------------------------------------------
render_en :: InterfaceText -> T.Text
render_en Start = "Please invite a friend to play Connect 4 or visit the"
    <> " Connect 4 Bot group https://telegram.me/Connect4BotGroup to find"
    <> " other players or to ask questions."

render_en Invite = "Invite"
render_en SendInvitation = "Invite this chat to a game of Connect 4"
render_en Invitation = "Would you like to play a game of Connect 4?"
render_en Yes = "Yes"
render_en No = "No"
render_en Rules = "Rules"

-- Deutsch -------------------------------------------------------------------
render_de :: InterfaceText -> T.Text
render_de Start = "Bitte lade einen Spieler zu einer Partie Vier Gewinnt ein"
    <> " oder besuche unsere Gruppe https://telegram.me/Connect4BotGroup um"
    <> " Spieler zu finden oder um Fragen zu stellen."

render_de Invite = "Spieler einladen"
render_de SendInvitation = "Einladung an diesen Chat verschicken"
render_de Invitation = "Moechtest Du eine Partie Vier Gewinnt spielen?"
render_de Yes = "Ja"
render_de No = "Nein"
render_de Rules = "Spielregeln"
