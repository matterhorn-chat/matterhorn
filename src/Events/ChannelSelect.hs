module Events.ChannelSelect where

import Brick
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State

onEventChannelSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventChannelSelect st e | Just kb <- lookupKeybinding e channelSelectKeybindings = kbAction kb st
onEventChannelSelect st (Vty.EvKey Vty.KBS []) = do
    continue $ updateChannelSelectMatches $ st & csChannelSelectString %~ (\s -> if T.null s then s else T.init s)
onEventChannelSelect st (Vty.EvKey (Vty.KChar c) []) | c /= '\t' = do
    continue $ updateChannelSelectMatches $ st & csChannelSelectString %~ (flip T.snoc c)
onEventChannelSelect st _ = do
    continue st

channelSelectKeybindings :: [Keybinding]
channelSelectKeybindings =
    [ KB "Select matching channel"
         (Vty.EvKey Vty.KEnter []) $
         \st -> do
             -- If there is only one channel selection match, switch to
             -- it
             let allMatches = (HM.elems $ st^.csChannelSelectChannelMatches) <>
                              (HM.elems $ st^.csChannelSelectUserMatches)
             continue =<< case allMatches of
                 [single] -> changeChannel (channelNameFromMatch single) $ st & csMode .~ Main
                 _        -> return st

    , KB "Cancel channel selection"
         (Vty.EvKey Vty.KEsc []) $
         \st -> continue $ st & csMode .~ Main
    ]
