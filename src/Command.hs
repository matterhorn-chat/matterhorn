{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Command where

import Brick (EventM, Next, continue, halt)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Lens.Micro.Platform

import Network.Mattermost
import Network.Mattermost.Lenses

import State
import Types

data CmdArgs :: * -> * where
  NoArg    :: CmdArgs ()
  LineArg  :: T.Text -> CmdArgs T.Text
  TokenArg :: T.Text -> CmdArgs rest -> CmdArgs (T.Text, rest)

type CmdExec a = a -> ChatState -> EventM Name (Next ChatState)

data Cmd = forall a. Cmd
  { cmdName    :: T.Text
  , cmdDescr   :: T.Text
  , cmdArgSpec :: CmdArgs a
  , cmdAction  :: CmdExec a
  }

commandName :: Cmd -> T.Text
commandName (Cmd name _ _ _ ) = name

printArgSpec :: CmdArgs a -> T.Text
printArgSpec NoArg = ""
printArgSpec (LineArg ts) = "[" <> ts <> "]"
printArgSpec (TokenArg t NoArg) = "[" <> t <> "]"
printArgSpec (TokenArg t rs) = "[" <> t <> "] " <> printArgSpec rs

matchArgs :: CmdArgs a -> [T.Text] -> Either T.Text a
matchArgs NoArg []  = return ()
matchArgs NoArg [t] = Left ("unexpected argument '" <> t <> "'")
matchArgs NoArg ts  = Left ("unexpected arguments '" <> T.unwords ts <> "'")
matchArgs (LineArg _) ts = return (T.unwords ts)
matchArgs rs@(TokenArg _ NoArg) [] = Left ("missing argument: " <> printArgSpec rs)
matchArgs rs@(TokenArg _ _) [] = Left ("missing arguments: " <> printArgSpec rs)
matchArgs (TokenArg _ rs) (t:ts) = (,) <$> pure t <*> matchArgs rs ts

commandList :: [Cmd]
commandList =
  [ Cmd "quit" "Exit Matterhorn" NoArg $ \ () st -> halt st
  , Cmd "right" "Focus on the next channel" NoArg $ \ () st ->
      nextChannel st >>= continue
  , Cmd "left" "Focus on the previous channel" NoArg $ \ () st ->
      prevChannel st >>= continue
  , Cmd "list-themes" "List the available themes" NoArg $ \ () st ->
      listThemes st >>= continue
  , Cmd "set-theme" "Set the color theme"
    (TokenArg "theme" NoArg) $ \ (themeName, ()) st ->
      setTheme st themeName >>= continue
  , Cmd "topic" "Set the current channel's topic"
    (LineArg "topic") $ \ p st -> do
      when (not $ T.null p) $ do
          liftIO $ setChannelTopic st p
      continue st
  , Cmd "focus" "Focus on a named channel"
    (TokenArg "channel" NoArg) $ \ (name, ()) st ->
      case channelByName st name of
        Just cId -> setFocus cId st >>= continue
        Nothing -> attemptCreateChannel name st >>= continue
  , Cmd "help" "Print the help dialogue" NoArg $ \ _ st -> do
          continue $ st & csMode .~ ShowHelp
  ]

dispatchCommand :: T.Text -> ChatState -> EventM Name (Next ChatState)
dispatchCommand cmd st =
  case T.words cmd of
    (x:xs) | [ c ] <- [ c | c@(Cmd name _ _ _) <- commandList
                          , name == x
                          ] ->
             case c of
               Cmd name _ spec exe ->
                 case matchArgs spec xs of
                  Left err   -> do
                    msg <- newClientMessage Error ("/" <> name <> ": " <> err)
                    addClientMessage msg st >>= continue
                  Right args -> exe args st
           | otherwise ->
             execMMCommand cmd st >>= continue
    _ -> continue st

setChannelTopic :: ChatState -> T.Text -> IO ()
setChannelTopic st msg = do
    let chanId = currentChannelId st
        theTeamId = st^.csMyTeam.teamIdL
    doAsyncWith st $ do
        void $ mmSetChannelHeader (st^.csConn) (st^.csTok) theTeamId chanId msg
        return $ \st' -> do
            return $ st' & msgMap.at chanId.each.ccInfo.cdHeader .~ msg
