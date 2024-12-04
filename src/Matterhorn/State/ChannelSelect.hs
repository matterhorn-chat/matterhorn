{-# LANGUAGE MultiWayIf #-}
module Matterhorn.State.ChannelSelect
  ( beginChannelSelect
  , updateChannelSelectMatches
  , channelSelectNext
  , channelSelectPrevious
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.Edit ( getEditContents )
import           Data.Char ( isUpper )
import qualified Data.Text as T
import           Lens.Micro.Platform

import qualified Network.Mattermost.Types as MM

import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z

beginChannelSelect :: MM.TeamId -> MH ()
beginChannelSelect tId = do
    pushMode tId ChannelSelect
    csTeam(tId).tsChannelSelectState .= emptyChannelSelectState tId
    updateChannelSelectMatches tId

    -- Preserve the current channel selection when initializing channel
    -- selection mode
    zipper <- use (csTeam(tId).tsFocus)
    let isCurrentFocus m = Just (matchEntry m) == Z.focus zipper
    csTeam(tId).tsChannelSelectState.channelSelectMatches %= Z.findRight isCurrentFocus

-- Select the next match in channel selection mode.
channelSelectNext :: MM.TeamId -> MH ()
channelSelectNext tId = updateSelectedMatch tId Z.right

-- Select the previous match in channel selection mode.
channelSelectPrevious :: MM.TeamId -> MH ()
channelSelectPrevious tId = updateSelectedMatch tId Z.left

updateChannelSelectMatches :: MM.TeamId -> MH ()
updateChannelSelectMatches tId = do
    st <- use id

    input <- use (csTeam(tId).tsChannelSelectState.channelSelectInput)
    cconfig <- use csClientConfig
    appConfig <- use (csResources.crConfiguration)
    prefs <- use (csResources.crUserPreferences)

    let pat = parseChannelSelectPattern appConfig $ T.concat $ getEditContents input
        chanNameMatches e = case pat of
            Nothing -> const Nothing
            Just p -> applySelectPattern appConfig p e
        patTy = case pat of
            Nothing -> Nothing
            Just CSPAny -> Nothing
            Just (CSP ty _) -> Just ty

    let chanMatches e chan =
            if patTy == Just PrefixDMOnly
            then Nothing
            else if chan^.ccInfo.cdType /= MM.Group
                 then chanNameMatches e $ chan^.ccInfo.cdDisplayName
                 else Nothing
        groupChanMatches e chan =
            if patTy == Just PrefixNonDMOnly
            then Nothing
            else if chan^.ccInfo.cdType == MM.Group
                 then chanNameMatches e $ chan^.ccInfo.cdDisplayName
                 else Nothing
        displayName uInfo = displayNameForUser uInfo cconfig prefs
        userMatches e uInfo =
            if patTy == Just PrefixNonDMOnly
            then Nothing
            else (chanNameMatches e . displayName) uInfo
        matches e =
            let cId = channelListEntryChannelId e
            in case channelListEntryType e of
                CLChannel    -> findChannelById cId (st^.csChannels) >>= chanMatches e
                CLUserDM uId -> userById uId st >>= userMatches e
                CLGroupDM    -> findChannelById cId (st^.csChannels) >>= groupChanMatches e

        preserveFocus Nothing _ = False
        preserveFocus (Just m) m2 = matchEntry m == matchEntry m2

    csTeam(tId).tsChannelSelectState.channelSelectMatches %=
        (Z.updateListBy preserveFocus $ Z.toList $ Z.maybeMapZipper matches (st^.csTeam(tId).tsFocus))

applySelectPattern :: Config -> ChannelSelectPattern -> ChannelListEntry -> Text -> Maybe ChannelSelectMatch
applySelectPattern _ CSPAny entry chanName = do
    return $ ChannelSelectMatch "" "" chanName chanName entry
applySelectPattern config (CSP ty pat) entry chanName = do
    let applyType Infix | pat `T.isInfixOf` normalizedChanName =
            case T.breakOn pat normalizedChanName of
                (pre, _) ->
                    return ( T.take (T.length pre) chanName
                           , T.take (T.length pat) $ T.drop (T.length pre) chanName
                           , T.drop (T.length pat + T.length pre) chanName
                           )

        applyType Prefix | pat `T.isPrefixOf` normalizedChanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType PrefixDMOnly | pat `T.isPrefixOf` normalizedChanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType PrefixNonDMOnly | pat `T.isPrefixOf` normalizedChanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType Suffix | pat `T.isSuffixOf` normalizedChanName = do
            let (b, a) = T.splitAt (T.length chanName - T.length pat) chanName
            return (b, a, "")

        applyType Equal  | pat == normalizedChanName =
            return ("", chanName, "")

        applyType _ = Nothing

        caseSensitive = T.any isUpper pat && not (configChannelSelectCaseInsensitive config)
        normalizedChanName = if caseSensitive
                             then chanName
                             else T.toLower chanName

    (pre, m, post) <- applyType ty
    return $ ChannelSelectMatch pre m post chanName entry

parseChannelSelectPattern :: Config -> Text -> Maybe ChannelSelectPattern
parseChannelSelectPattern _ "" = return CSPAny
parseChannelSelectPattern config patRaw = do
    let pat = if configChannelSelectCaseInsensitive config
              then T.toLower patRaw
              else patRaw

        only = if | userSigil `T.isPrefixOf` pat -> Just $ CSP PrefixDMOnly $ T.tail pat
                  | normalChannelSigil `T.isPrefixOf` pat -> Just $ CSP PrefixNonDMOnly $ T.tail pat
                  | otherwise -> Nothing

    (pat1, pfx) <- case "^" `T.isPrefixOf` pat of
        True  -> return (T.tail pat, Just Prefix)
        False -> return (pat, Nothing)

    (pat2, sfx) <- case "$" `T.isSuffixOf` pat1 of
        True  -> return (T.init pat1, Just Suffix)
        False -> return (pat1, Nothing)

    only <|> case (pfx, sfx) of
        (Nothing, Nothing)         -> return $ CSP Infix  pat2
        (Just Prefix, Nothing)     -> return $ CSP Prefix pat2
        (Nothing, Just Suffix)     -> return $ CSP Suffix pat2
        (Just Prefix, Just Suffix) -> return $ CSP Equal  pat2
        tys                        -> error $ "BUG: invalid channel select case: " <> show tys

-- Update the channel selection mode match cursor. The argument function
-- determines how to navigate to the next item.
updateSelectedMatch :: MM.TeamId
                    -> (Z.Zipper ChannelListGroup ChannelSelectMatch -> Z.Zipper ChannelListGroup ChannelSelectMatch)
                    -> MH ()
updateSelectedMatch tId nextItem =
    csTeam(tId).tsChannelSelectState.channelSelectMatches %= nextItem
