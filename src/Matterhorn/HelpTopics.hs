{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.HelpTopics
  ( helpTopics
  , lookupHelpTopic
  , themeHelpTopic
  , mainHelpTopic
  )
where

import Prelude ()
import Matterhorn.Prelude

import Matterhorn.Types


helpTopics :: [HelpTopic]
helpTopics =
    [ mainHelpTopic
    , scriptHelpTopic
    , themeHelpTopic
    , keybindingHelpTopic
    , syntaxHighlightingHelpTopic
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

keybindingHelpTopic :: HelpTopic
keybindingHelpTopic =
    HelpTopic "keybindings" "Help on overriding keybindings"
      KeybindingHelp KeybindingHelpText

syntaxHighlightingHelpTopic :: HelpTopic
syntaxHighlightingHelpTopic =
    HelpTopic "syntax" "Help on syntax highlighing"
      SyntaxHighlightHelp SyntaxHighlightHelpText

lookupHelpTopic :: Text -> Maybe HelpTopic
lookupHelpTopic topic =
    listToMaybe $ filter ((== topic) . helpTopicName) helpTopics
