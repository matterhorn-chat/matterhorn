module Types.KeyEvents
  (
  -- * Types
    KeyEvent(..)
  , KeyConfig
  , Binding(..)

  -- * Data
  , allEvents

  -- * Parsing and pretty-printing
  , parseBinding
  , parseBindingList
  , ppBinding

  -- * Key event name resolution
  , keyEventFromName
  , keyEventName
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)
import Data.Monoid ((<>))

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
  | NextUnreadChannelEvent
  | LastChannelEvent
  | EnterOpenURLModeEvent
  | ClearUnreadEvent
  | ToggleMultiLineEvent
  | EnterFlaggedPostsEvent

  -- generic cancel
  | CancelEvent

  -- channel-scroll-specific
  | LoadMoreEvent
  | OpenMessageURLEvent

  -- scrolling events---maybe rebindable?
  | ScrollUpEvent
  | ScrollDownEvent
  | PageUpEvent
  | PageDownEvent
  | ScrollTopEvent
  | ScrollBottomEvent

  -- select events---not the same as scrolling sometimes!
  | SelectUpEvent
  | SelectDownEvent

  | FlagMessageEvent
  | YankMessageEvent
  | DeleteMessageEvent
  | EditMessageEvent
  | ReplyMessageEvent
    deriving (Eq, Show, Ord, Enum)

allEvents :: [KeyEvent]
allEvents =
  [ VtyRefreshEvent
  , ShowHelpEvent
  , EnterSelectModeEvent
  , ReplyRecentEvent
  , ToggleMessagePreviewEvent
  , InvokeEditorEvent
  , EnterFastSelectModeEvent
  , QuitEvent
  , NextChannelEvent
  , PrevChannelEvent
  , NextUnreadChannelEvent
  , LastChannelEvent
  , EnterOpenURLModeEvent
  , ClearUnreadEvent
  , ToggleMultiLineEvent
  , CancelEvent
  , EnterFlaggedPostsEvent

  , LoadMoreEvent
  , OpenMessageURLEvent

  , SelectUpEvent
  , SelectDownEvent

  , FlagMessageEvent
  , YankMessageEvent
  , DeleteMessageEvent
  , EditMessageEvent
  , ReplyMessageEvent
  ]

data Binding = Binding
  { kbMods :: [Vty.Modifier]
  , kbKey  :: Vty.Key
  } deriving (Eq, Show, Ord)

type KeyConfig = M.Map KeyEvent [Binding]

parseBinding :: T.Text -> Either String Binding
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
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

ppBinding :: Binding -> T.Text
ppBinding (Binding mods k) =
    T.intercalate "-" $ (ppMod <$> mods) <> [ppKey k]

ppKey :: Vty.Key -> T.Text
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

ppChar :: Char -> T.Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppMod :: Vty.Modifier -> T.Text
ppMod Vty.MMeta  = "M"
ppMod Vty.MAlt   = "A"
ppMod Vty.MCtrl  = "C"
ppMod Vty.MShift = "S"

parseBindingList :: T.Text -> Either String [Binding]
parseBindingList =
  mapM (parseBinding . T.strip) . T.splitOn ","

keyEventFromName :: T.Text -> Either String KeyEvent
keyEventFromName t =
    let mapping = M.fromList [ (keyEventName e, e) | e <- allEvents ]
    in case M.lookup t mapping of
        Just e -> return e
        Nothing -> Left ("Unknown event: " ++ show t)

keyEventName :: KeyEvent -> T.Text
keyEventName ev = case ev of
  QuitEvent                 -> "quit"
  VtyRefreshEvent           -> "vty-refresh"
  ClearUnreadEvent          -> "clear-unread"

  ToggleMessagePreviewEvent -> "toggle-message-preview"
  InvokeEditorEvent         -> "invoke-editor"
  ToggleMultiLineEvent      -> "toggle-multiline"
  CancelEvent               -> "cancel"
  ReplyRecentEvent          -> "reply-recent"

  EnterFastSelectModeEvent  -> "enter-fast-select"
  NextChannelEvent          -> "focus-next-channel"
  PrevChannelEvent          -> "focus-prev-channel"
  NextUnreadChannelEvent    -> "focus-next-unread"
  LastChannelEvent          -> "focus-last-channel"

  EnterFlaggedPostsEvent    -> "show-flagged-posts"
  ShowHelpEvent             -> "show-help"
  EnterSelectModeEvent      -> "select-mode"
  EnterOpenURLModeEvent     -> "enter-url-open"

  LoadMoreEvent             -> "load-more"
  OpenMessageURLEvent       -> "open-message-url"

  ScrollUpEvent     -> "scroll-up"
  ScrollDownEvent   -> "scroll-down"
  PageUpEvent       -> "page-up"
  PageDownEvent     -> "page-down"
  ScrollTopEvent    -> "scroll-top"
  ScrollBottomEvent -> "scroll-bottom"

  SelectUpEvent   -> "select-up"
  SelectDownEvent -> "select-down"

  FlagMessageEvent   -> "flag-message"
  YankMessageEvent   -> "yank-message"
  DeleteMessageEvent -> "delete-message"
  EditMessageEvent   -> "edit-message"
  ReplyMessageEvent  -> "reply-recent"
