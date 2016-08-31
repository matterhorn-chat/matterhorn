{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}

module Draw where

import           Brick
import           Brick.Markup (markup, (@?))
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit (renderEditor)
import qualified Data.Text as T
import qualified Data.Array as A
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime )
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ( HashMap )
import           Data.List (sortBy, intercalate)
import           Data.Ord (comparing)
import           Data.Maybe ( listToMaybe, maybeToList )
import           Data.Monoid ((<>))
import           Lens.Micro.Platform

import "text-markup" Data.Text.Markup
import           Text.Regex.Base.RegexLike (makeRegex, matchAll)
import           Text.Regex.TDFA.String

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           State
import           Themes
import           Types
import           DrawUtil

-- If the config's date format is not set.
defaultDateFormat :: String
defaultDateFormat = "%R"

renderTime :: String -> TimeZone -> UTCTime -> Widget Name
renderTime fmt tz t =
    let timeStr = formatTime defaultTimeLocale fmt (utcToLocalTime tz t)
    in str "[" <+> withDefAttr timeAttr (str timeStr) <+> str "]"

emailPattern :: Regex
emailPattern = makeRegex ("[[:alnum:]\\+]+@([[:alnum:]]+\\.)+([[:alnum:]]+)"::String)

urlPattern :: Regex
urlPattern = makeRegex ("https?://([[:alnum:]]+\\.)*([[:alnum:]]+)(:[[:digit:]]+)?(/[^[:space:]]*)"::String)

markdownPattern :: Regex
markdownPattern = makeRegex ("`[^`]+`"::String)

mkUsernamePattern :: ChatState -> Regex
mkUsernamePattern cs =
    let users = cs ^. usrMap & HM.elems
    in makeRegex $ "(@|\\b)(" ++ intercalate "|" ((^.userProfileUsernameL) <$> users) ++ ")\\b"

findRegex :: T.Text -> Regex -> [(Int, Int)]
findRegex t r = concat $ A.elems <$> matchAll r (T.unpack t)

doMessageMarkup :: Regex -> T.Text -> Widget a
doMessageMarkup usernamePattern msg =
    let emailMatches    = findRegex msg emailPattern
        urlMatches      = findRegex msg urlPattern
        markdownMatches = findRegex msg markdownPattern
        usernameMatches = findRegex msg usernamePattern
        substr pos len s = T.take len $ T.drop pos s
        applyUsernameMatches mkup = foldr markUsername mkup usernameMatches
        markUsername (pos,len) m =
            let tag = attrForUsername (T.unpack $ substr pos len msg)
            in markRegion pos len tag m
        applyMatches matches tag mkup = foldr (\(pos,len) -> markRegion pos len tag) mkup matches

        pairs = fromMarkup $ applyMatches emailMatches    emailAttr    $
                             applyMatches markdownMatches markdownAttr $
                             applyMatches urlMatches      urlAttr      $
                             applyUsernameMatches                      $
                             toMarkup msg ""
    in markup $ mconcat $ (uncurry (@?)) <$> pairs

renderChatMessage :: Regex -> Maybe String -> TimeZone -> Int -> (Int, (UTCTime, String, String, Bool)) -> Widget Name
renderChatMessage uPattern mFormat tz lastIdx (i, (t, u, m, isEmotePost)) =
    let f = if i == lastIdx
            then visible
            else id
        doFormat prefix wrapped =
            let suffix = drop (length u + length prefix) wrapped
                (first, rest) = case lines suffix of
                    [] -> ("", [])
                    (fl:r) -> (fl, r)
                firstLine = str prefix <+> colorUsername u <+> doMessageMarkup uPattern (T.pack first)
            in case rest of
                 [] -> firstLine
                 _ -> vBox $ firstLine : (doMessageMarkup uPattern <$> T.pack <$> rest)
        msg = case isEmotePost of
               True -> wrappedText (doFormat "*") ("*" ++ u ++ " " ++ (init $ tail m))
               False -> wrappedText (doFormat "")  (u ++ ": " ++ m)
    in f $ case mFormat of
        Just ""     -> msg
        Just format -> renderTime format tz t            <+> str " " <+> msg
        Nothing     -> renderTime defaultDateFormat tz t <+> str " " <+> msg

mkChannelName :: String -> String
mkChannelName = ('#':)

mkDMChannelName :: String -> String
mkDMChannelName = ('@':)

channelListWidth :: Int
channelListWidth = 20

normalChannelListHeight :: Int
normalChannelListHeight = 10

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $ vBox
                       [ header "Channels"
                       , vLimit normalChannelListHeight $ viewport NormalChannelList Vertical $ vBox channelNames
                       , header "Users"
                       , viewport DMChannelList Vertical $ vBox dmChannelNames
                       ]
    where
    cId = currentChannelId st
    currentChannelName = getChannelName cId st
    header label = hBorderWithLabel $
                   withDefAttr channelListHeaderAttr $
                   str label
    channelNames = [ attr $ padRight Max $ str (indicator ++ mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let indicator = if | unread    -> "!"
                                        | otherwise -> " "
                         attr = if current
                                then visible . withDefAttr currentChannelNameAttr
                                else id
                         current = n == currentChannelName
                         Just chan = st ^. csNames . cnToChanId . at n
                         unread = hasUnread st chan
                   ]
    dmChannelNames = [ attr $ padRight Max $ str indicator <+> colorUsername (mkDMChannelName (u^.userProfileUsernameL))
                     | u <- sortBy (comparing userProfileUsername) (st ^. usrMap & HM.elems)
                     , let indicator = if | unread    -> "!"
                                          | otherwise -> " "
                           attr = if current
                                  then visible . forceAttr currentChannelNameAttr
                                  else id
                           cname = getDMChannelName (st^.csMe^.userIdL)
                                                    (u^.userProfileIdL)
                           current = cname == currentChannelName
                           m_chanId = st^.csNames.cnToChanId.at (userProfileUsername u)
                           unread = maybe False (hasUnread st) m_chanId
                     ]

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st = prompt <+> inputBox
    where
    prompt = str "> "
    inputBox = renderEditor True (st^.cmdLine)

renderCurrentChannelDisplay :: ChatState -> Widget Name
renderCurrentChannelDisplay st = header <=> messages
    where
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             case null purposeStr of
                 True -> case chnType of
                   Type "D" ->
                     case findUserByDMChannelName (st^.usrMap)
                                                  chnName
                                                  (st^.csMe^.userIdL) of
                       Nothing -> str $ mkChannelName chnName
                       Just u  -> colorUsername $ mkDMChannelName (u^.userProfileUsernameL)
                   _        -> str $ mkChannelName chnName
                 False -> wrappedText str $ mkChannelName chnName <> " - " <> purposeStr
    messages = viewport (ChannelMessages cId) Vertical chatText <+> str " "
    uPattern = mkUsernamePattern st
    chatText = vBox $ renderChatMessage uPattern (st ^. timeFormat) (st ^. timeZone) (length channelMessages - 1) <$>
                      zip [0..] channelMessages
    channelMessages = getMessageListing cId st
    cId = currentChannelId st
    Just chan = getChannel cId st
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    purposeStr = chan^.ccInfo.cdPurpose

findUserByDMChannelName :: HashMap UserId UserProfile
                        -> String -- ^ the dm channel name
                        -> UserId -- ^ me
                        -> Maybe UserProfile -- ^ you
findUserByDMChannelName userMap dmchan me = listToMaybe
  [ user
  | u <- HM.keys userMap
  , getDMChannelName me u == dmchan
  , user <- maybeToList (HM.lookup u userMap)
  ]

chatDraw :: ChatState -> [Widget Name]
chatDraw st =
    [ (renderChannelList st <+> (borderElem bsIntersectR <=>
                                 vLimit normalChannelListHeight vBorder <=>
                                 borderElem bsIntersectR <=> vBorder)
                            <+> renderCurrentChannelDisplay st)
      <=> (hLimit channelListWidth hBorder <+> borderElem bsIntersectB <+> hBorder)
      <=> renderUserCommandBox st
    ]
