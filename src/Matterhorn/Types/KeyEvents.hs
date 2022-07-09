module Matterhorn.Types.KeyEvents
  (
  -- * Types
    KeyEvent(..)
  , KeyConfig(keyConfigEvents)
  , Binding(..)
  , BindingState(..)
  , lookupKeyConfigBindings
  , newKeyConfig
  , getFirstDefaultBinding
  , firstActiveBinding
  , allDefaultBindings

  -- * Data
  , allEvents

  -- * Parsing and pretty-printing
  , parseBinding
  , parseBindingList
  , ppBinding
  , ppMaybeBinding
  , nonCharKeys
  , eventToBinding

  -- * Key event collection
  , KeyEvents
  , keyEvents
  , keyEventsList
  , lookupKeyEvent
  , keyEventName
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Bimap as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty


-- | This enum represents all the possible key events a user might
--   want to use.
data KeyEvent
  = VtyRefreshEvent
  | ShowHelpEvent
  | EnterSelectModeEvent
  | ReplyRecentEvent
  | ToggleMessagePreviewEvent
  | InvokeEditorEvent
  | EnterFastSelectModeEvent
  | QuitEvent
  | NextChannelEvent
  | PrevChannelEvent
  | NextChannelEventAlternate
  | PrevChannelEventAlternate
  | NextUnreadChannelEvent
  | NextUnreadUserOrChannelEvent
  | LastChannelEvent
  | EnterOpenURLModeEvent
  | ClearUnreadEvent
  | ToggleMultiLineEvent
  | EnterFlaggedPostsEvent
  | ToggleChannelListVisibleEvent
  | ToggleExpandedChannelTopicsEvent
  | ShowAttachmentListEvent
  | ChangeMessageEditorFocus

  | EditorKillToBolEvent
  | EditorKillToEolEvent
  | EditorBolEvent
  | EditorEolEvent
  | EditorTransposeCharsEvent
  | EditorDeleteCharacter
  | EditorPrevCharEvent
  | EditorNextCharEvent
  | EditorPrevWordEvent
  | EditorNextWordEvent
  | EditorDeleteNextWordEvent
  | EditorDeletePrevWordEvent
  | EditorHomeEvent
  | EditorEndEvent
  | EditorYankEvent

  | CycleChannelListSorting

  | SelectNextTabEvent
  | SelectPreviousTabEvent

  | SaveAttachmentEvent

  -- generic cancel
  | CancelEvent

  -- channel-scroll-specific
  | LoadMoreEvent
  | OpenMessageURLEvent

  -- scrolling events---maybe rebindable?
  | ScrollUpEvent
  | ScrollDownEvent
  | ScrollLeftEvent
  | ScrollRightEvent
  | PageUpEvent
  | PageDownEvent
  | PageRightEvent
  | PageLeftEvent
  | ScrollTopEvent
  | ScrollBottomEvent
  | SelectOldestMessageEvent
  | ChannelListScrollUpEvent
  | ChannelListScrollDownEvent

  -- select events---not the same as scrolling sometimes!
  | SelectUpEvent
  | SelectDownEvent

  -- search select events---these need to not be valid editor inputs
  -- (such as 'j' and 'k')
  | SearchSelectUpEvent
  | SearchSelectDownEvent

  -- E.g. Pressing enter on an item in a list to do something with it
  | ActivateListItemEvent

  | ViewMessageEvent
  | FillGapEvent
  | CopyPostLinkEvent
  | FlagMessageEvent
  | OpenThreadEvent
  | PinMessageEvent
  | YankMessageEvent
  | YankWholeMessageEvent
  | DeleteMessageEvent
  | EditMessageEvent
  | ReplyMessageEvent
  | ReactToMessageEvent

  -- Attachments
  | AttachmentListAddEvent
  | AttachmentListDeleteEvent
  | AttachmentOpenEvent

  -- Attachment file browser
  | FileBrowserBeginSearchEvent
  | FileBrowserSelectEnterEvent
  | FileBrowserSelectCurrentEvent
  | FileBrowserListPageUpEvent
  | FileBrowserListPageDownEvent
  | FileBrowserListHalfPageUpEvent
  | FileBrowserListHalfPageDownEvent
  | FileBrowserListTopEvent
  | FileBrowserListBottomEvent
  | FileBrowserListNextEvent
  | FileBrowserListPrevEvent


  -- Form submission
  | FormSubmitEvent

  -- Team switching
  | NextTeamEvent
  | PrevTeamEvent
  | MoveCurrentTeamLeftEvent
  | MoveCurrentTeamRightEvent
    deriving (Eq, Show, Ord, Enum)

data KeyEvents e = KeyEvents (B.Bimap Text e)
                 deriving (Eq, Show)

keyEvents :: (Ord e) => [(Text, e)] -> KeyEvents e
keyEvents pairs = KeyEvents $ B.fromList pairs

keyEventsList :: KeyEvents e -> [(Text, e)]
keyEventsList (KeyEvents m) = B.toList m

allEvents :: KeyEvents KeyEvent
allEvents =
    keyEvents
    [ ("quit", QuitEvent)
    , ("vty-refresh", VtyRefreshEvent)
    , ("clear-unread", ClearUnreadEvent)
    , ("cancel", CancelEvent)
    , ("toggle-message-preview", ToggleMessagePreviewEvent)
    , ("invoke-editor", InvokeEditorEvent)
    , ("toggle-multiline", ToggleMultiLineEvent)
    , ("reply-recent", ReplyRecentEvent)
    , ("enter-fast-select", EnterFastSelectModeEvent)
    , ("focus-next-channel", NextChannelEvent)
    , ("focus-prev-channel", PrevChannelEvent)
    , ("focus-next-channel-alternate", NextChannelEventAlternate)
    , ("focus-prev-channel-alternate", PrevChannelEventAlternate)
    , ("focus-next-unread", NextUnreadChannelEvent)
    , ("focus-next-unread-user-or-channel", NextUnreadUserOrChannelEvent)
    , ("focus-last-channel", LastChannelEvent)
    , ("select-next-tab", SelectNextTabEvent)
    , ("select-previous-tab", SelectPreviousTabEvent)
    , ("save-attachment", SaveAttachmentEvent)
    , ("show-attachment-list", ShowAttachmentListEvent)
    , ("change-message-editor-focus", ChangeMessageEditorFocus)
    , ("editor-kill-to-beginning-of-line", EditorKillToBolEvent)
    , ("editor-kill-to-end-of-line", EditorKillToEolEvent)
    , ("editor-beginning-of-line", EditorBolEvent)
    , ("editor-end-of-line", EditorEolEvent)
    , ("editor-transpose-chars", EditorTransposeCharsEvent)
    , ("editor-delete-char", EditorDeleteCharacter)
    , ("editor-prev-char", EditorPrevCharEvent)
    , ("editor-next-char", EditorNextCharEvent)
    , ("editor-prev-word", EditorPrevWordEvent)
    , ("editor-next-word", EditorNextWordEvent)
    , ("editor-delete-next-word", EditorDeleteNextWordEvent)
    , ("editor-delete-prev-word", EditorDeletePrevWordEvent)
    , ("editor-home", EditorHomeEvent)
    , ("editor-end", EditorEndEvent)
    , ("editor-yank", EditorYankEvent)
    , ("cycle-channel-list-sorting", CycleChannelListSorting)
    , ("next-team", NextTeamEvent)
    , ("prev-team", PrevTeamEvent)
    , ("move-current-team-left", MoveCurrentTeamLeftEvent)
    , ("move-current-team-right", MoveCurrentTeamRightEvent)
    , ("show-flagged-posts", EnterFlaggedPostsEvent)
    , ("toggle-channel-list-visibility", ToggleChannelListVisibleEvent)
    , ("toggle-expanded-channel-topics", ToggleExpandedChannelTopicsEvent)
    , ("show-help", ShowHelpEvent)
    , ("select-mode", EnterSelectModeEvent)
    , ("enter-url-open", EnterOpenURLModeEvent)
    , ("load-more", LoadMoreEvent)
    , ("open-message-url", OpenMessageURLEvent)
    , ("scroll-up", ScrollUpEvent)
    , ("scroll-down", ScrollDownEvent)
    , ("scroll-left", ScrollLeftEvent)
    , ("scroll-right", ScrollRightEvent)
    , ("channel-list-scroll-up", ChannelListScrollUpEvent)
    , ("channel-list-scroll-down", ChannelListScrollDownEvent)
    , ("page-up", PageUpEvent)
    , ("page-down", PageDownEvent)
    , ("page-left", PageLeftEvent)
    , ("page-right", PageRightEvent)
    , ("scroll-top", ScrollTopEvent)
    , ("scroll-bottom", ScrollBottomEvent)
    , ("select-oldest-message", SelectOldestMessageEvent)
    , ("select-up", SelectUpEvent)
    , ("select-down", SelectDownEvent)
    , ("search-select-up", SearchSelectUpEvent)
    , ("search-select-down", SearchSelectDownEvent)
    , ("activate-list-item", ActivateListItemEvent)
    , ("open-thread", OpenThreadEvent)
    , ("flag-message", FlagMessageEvent)
    , ("pin-message", PinMessageEvent)
    , ("view-message", ViewMessageEvent)
    , ("fetch-for-gap", FillGapEvent)
    , ("copy-post-link", CopyPostLinkEvent)
    , ("yank-message", YankMessageEvent)
    , ("yank-whole-message", YankWholeMessageEvent)
    , ("delete-message", DeleteMessageEvent)
    , ("edit-message", EditMessageEvent)
    , ("reply-message", ReplyMessageEvent)
    , ("react-to-message", ReactToMessageEvent)
    , ("add-to-attachment-list", AttachmentListAddEvent)
    , ("delete-from-attachment-list", AttachmentListDeleteEvent)
    , ("open-attachment", AttachmentOpenEvent)
    , ("filebrowser-begin-search", FileBrowserBeginSearchEvent)
    , ("filebrowser-select-file-or-enter-directory", FileBrowserSelectEnterEvent)
    , ("filebrowser-select-current", FileBrowserSelectCurrentEvent)
    , ("filebrowser-list-page-up", FileBrowserListPageUpEvent)
    , ("filebrowser-list-page-down", FileBrowserListPageDownEvent)
    , ("filebrowser-list-half-page-up", FileBrowserListHalfPageUpEvent)
    , ("filebrowser-list-half-page-down", FileBrowserListHalfPageDownEvent)
    , ("filebrowser-list-top", FileBrowserListTopEvent)
    , ("filebrowser-list-bottom", FileBrowserListBottomEvent)
    , ("filebrowser-list-next", FileBrowserListNextEvent)
    , ("filebrowser-list-previous", FileBrowserListPrevEvent)
    , ("submit-form", FormSubmitEvent)
    ]

eventToBinding :: Vty.Event -> Binding
eventToBinding (Vty.EvKey k mods) = Binding mods k
eventToBinding k = error $ "BUG: invalid keybinding " <> show k

data Binding =
    Binding { kbMods :: [Vty.Modifier]
            , kbKey  :: Vty.Key
            } deriving (Eq, Show, Ord)

data BindingState =
    BindingList [Binding]
    | Unbound
    deriving (Show, Eq, Ord)

-- | A configuration of custom key bindings.
data KeyConfig e =
    KeyConfig { keyConfigBindingMap :: M.Map e BindingState
              -- ^ The map of custom bindings for events with custom
              -- bindings
              , keyConfigEvents :: KeyEvents e
              -- ^ The base mapping of events and their names that is
              -- used in this configuration
              , keyConfigDefaultBindings :: M.Map e [Binding]
              -- ^ A mapping of events and their default key bindings,
              -- if any
              }
              deriving (Show, Eq)

newKeyConfig :: (Ord e)
             => KeyEvents e
             -- ^ The base mapping of key events to use
             -> [(e, BindingState)]
             -- ^ Custom bindings by key event
             -> [(e, [Binding])]
             -- ^ Default bindings by key event
             -> KeyConfig e
newKeyConfig evs bindings defaults =
    KeyConfig { keyConfigBindingMap = M.fromList bindings
              , keyConfigEvents = evs
              , keyConfigDefaultBindings = M.fromList defaults
              }

getFirstDefaultBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
getFirstDefaultBinding kc ev = do
    bs <- M.lookup ev (keyConfigDefaultBindings kc)
    case bs of
        (b:_) -> Just b
        _ -> Nothing

allDefaultBindings :: (Ord e) => KeyConfig e -> e -> [Binding]
allDefaultBindings kc ev =
    fromMaybe [] $ M.lookup ev (keyConfigDefaultBindings kc)

firstActiveBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
firstActiveBinding kc ev = foundBinding <|> defaultBinding
    where
        defaultBinding = getFirstDefaultBinding kc ev
        foundBinding = do
            bState <- lookupKeyConfigBindings kc ev
            case bState of
                BindingList (b:_) -> Just b
                _ -> Nothing

lookupKeyConfigBindings :: (Ord e) => KeyConfig e -> e -> Maybe BindingState
lookupKeyConfigBindings kc e = M.lookup e $ keyConfigBindingMap kc

parseBinding :: Text -> Either String Binding
parseBinding kb = go (T.splitOn "-" $ T.toLower kb) []
  where go [k] mods = do
          key <- pKey k
          return Binding { kbMods = mods, kbKey = key }
        go (k:ks) mods = do
          m <- case k of
            "s"       -> return Vty.MShift
            "shift"   -> return Vty.MShift
            "m"       -> return Vty.MMeta
            "meta"    -> return Vty.MMeta
            "a"       -> return Vty.MAlt
            "alt"     -> return Vty.MAlt
            "c"       -> return Vty.MCtrl
            "ctrl"    -> return Vty.MCtrl
            "control" -> return Vty.MCtrl
            _         -> Left ("Unknown modifier prefix: " ++ show k)
          go ks (m:mods)
        go [] _ = Left "Empty keybinding not allowed"
        pKey "esc"       = return Vty.KEsc
        pKey "backspace" = return Vty.KBS
        pKey "enter"     = return Vty.KEnter
        pKey "left"      = return Vty.KLeft
        pKey "right"     = return Vty.KRight
        pKey "up"        = return Vty.KUp
        pKey "down"      = return Vty.KDown
        pKey "upleft"    = return Vty.KUpLeft
        pKey "upright"   = return Vty.KUpRight
        pKey "downleft"  = return Vty.KDownLeft
        pKey "downright" = return Vty.KDownRight
        pKey "center"    = return Vty.KCenter
        pKey "backtab"   = return Vty.KBackTab
        pKey "printscreen" = return Vty.KPrtScr
        pKey "pause"     = return Vty.KPause
        pKey "insert"    = return Vty.KIns
        pKey "home"      = return Vty.KHome
        pKey "pgup"      = return Vty.KPageUp
        pKey "del"       = return Vty.KDel
        pKey "end"       = return Vty.KEnd
        pKey "pgdown"    = return Vty.KPageDown
        pKey "begin"     = return Vty.KBegin
        pKey "menu"      = return Vty.KMenu
        pKey "space"     = return (Vty.KChar ' ')
        pKey "tab"       = return (Vty.KChar '\t')
        pKey t
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

ppBinding :: Binding -> Text
ppBinding (Binding mods k) =
    T.intercalate "-" $ (ppMod <$> mods) <> [ppKey k]

ppMaybeBinding :: Maybe Binding -> Text
ppMaybeBinding Nothing =
    "(no binding)"
ppMaybeBinding (Just b) =
    ppBinding b

ppKey :: Vty.Key -> Text
ppKey (Vty.KChar c)   = ppChar c
ppKey (Vty.KFun n)    = "F" <> (T.pack $ show n)
ppKey Vty.KBackTab    = "BackTab"
ppKey Vty.KEsc        = "Esc"
ppKey Vty.KBS         = "Backspace"
ppKey Vty.KEnter      = "Enter"
ppKey Vty.KUp         = "Up"
ppKey Vty.KDown       = "Down"
ppKey Vty.KLeft       = "Left"
ppKey Vty.KRight      = "Right"
ppKey Vty.KHome       = "Home"
ppKey Vty.KEnd        = "End"
ppKey Vty.KPageUp     = "PgUp"
ppKey Vty.KPageDown   = "PgDown"
ppKey Vty.KDel        = "Del"
ppKey Vty.KUpLeft     = "UpLeft"
ppKey Vty.KUpRight    = "UpRight"
ppKey Vty.KDownLeft   = "DownLeft"
ppKey Vty.KDownRight  = "DownRight"
ppKey Vty.KCenter     = "Center"
ppKey Vty.KPrtScr     = "PrintScreen"
ppKey Vty.KPause      = "Pause"
ppKey Vty.KIns        = "Insert"
ppKey Vty.KBegin      = "Begin"
ppKey Vty.KMenu       = "Menu"

nonCharKeys :: [Text]
nonCharKeys = map ppKey
  [ Vty.KBackTab, Vty.KEsc, Vty.KBS, Vty.KEnter, Vty.KUp, Vty.KDown
  , Vty.KLeft, Vty.KRight, Vty.KHome, Vty.KEnd, Vty.KPageDown
  , Vty.KPageUp, Vty.KDel, Vty.KUpLeft, Vty.KUpRight, Vty.KDownLeft
  , Vty.KDownRight, Vty.KCenter, Vty.KPrtScr, Vty.KPause, Vty.KIns
  , Vty.KBegin, Vty.KMenu
  ]

ppChar :: Char -> Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppMod :: Vty.Modifier -> Text
ppMod Vty.MMeta  = "M"
ppMod Vty.MAlt   = "A"
ppMod Vty.MCtrl  = "C"
ppMod Vty.MShift = "S"

parseBindingList :: Text -> Either String BindingState
parseBindingList t =
    if T.toLower t == "unbound"
    then return Unbound
    else BindingList <$> mapM (parseBinding . T.strip) (T.splitOn "," t)

lookupKeyEvent :: (Ord e) => KeyEvents e -> Text -> Maybe e
lookupKeyEvent (KeyEvents m) name = B.lookup name m

keyEventName :: (Ord e) => KeyEvents e -> e -> Maybe Text
keyEventName (KeyEvents m) e = B.lookupR e m
