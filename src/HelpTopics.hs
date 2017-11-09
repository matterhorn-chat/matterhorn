{-# LANGUAGE OverloadedStrings #-}
module HelpTopics
  ( helpTopics
  , lookupHelpTopic
  , themeHelpTopic

  , mainHelpTopic
  )
where

import Prelude ()
import Prelude.Compat

import qualified Data.Text as T
import Data.Maybe (listToMaybe)

import Types

helpTopics :: [HelpTopic]
helpTopics =
    [ mainHelpTopic
    , scriptHelpTopic
    , themeHelpTopic
    ]

mainHelpTopic :: HelpTopic
mainHelpTopic =
    HelpTopic "main" "This help page" MainHelp HelpText

scriptHelpTopic :: HelpTopic
scriptHelpTopic =
    HelpTopic "scripts" "Help on available scripts" ScriptHelp ScriptHelpText

themeHelpTopic :: HelpTopic
themeHelpTopic =
    HelpTopic "themes" "Help on color themes" ThemeHelp ThemeHelpText

lookupHelpTopic :: T.Text -> Maybe HelpTopic
lookupHelpTopic topic =
    listToMaybe $ filter ((== topic) . helpTopicName) helpTopics
