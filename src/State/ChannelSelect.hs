{-# LANGUAGE MultiWayIf #-}
module State.ChannelSelect
  ( beginChannelSelect
  , updateChannelSelectMatches
  , channelSelectNext
  , channelSelectPrevious
  )
where

import           Prelude ()
import           Prelude.MH

import           Data.List ( findIndex )
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Types


beginChannelSelect :: MH ()
beginChannelSelect = do
    setMode ChannelSelect
    csChannelSelectState .= emptyChannelSelectState

-- Select the next match in channel selection mode.
channelSelectNext :: MH ()
channelSelectNext = updateSelectedMatch succ

-- Select the previous match in channel selection mode.
channelSelectPrevious :: MH ()
channelSelectPrevious = updateSelectedMatch pred

updateChannelSelectMatches :: MH ()
updateChannelSelectMatches = do
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    input <- use (csChannelSelectState.channelSelectInput)
    let pat = parseChannelSelectPattern input
        chanNameMatches = case pat of
            Nothing -> const Nothing
            Just p -> if T.null input
                      then const Nothing
                      else applySelectPattern p
        patTy = case pat of
            Nothing -> Nothing
            Just (CSP ty _) -> Just ty

    chanNames   <- gets (sort . allChannelNames)
    uList       <- use (to sortedUserList)
    displayNick <- use (to useNickname)
    let chanMatches = if patTy == Just UsersOnly
                      then mempty
                      else catMaybes (fmap chanNameMatches chanNames)
        displayName uInf
            | displayNick = uInf^.uiNickName.non (uInf^.uiName)
            | otherwise   = uInf^.uiName
        usernameMatches = if patTy == Just ChannelsOnly
                          then mempty
                          else catMaybes (fmap (chanNameMatches . displayName) uList)

    newInput <- use (csChannelSelectState.channelSelectInput)
    csChannelSelectState.channelMatches .= chanMatches
    csChannelSelectState.userMatches    .= usernameMatches
    csChannelSelectState.selectedMatch  %= \oldMatch ->
        -- If the user input exactly matches one of the matches, prefer
        -- that one. Otherwise, if the previously selected match is
        -- still a possible match, leave it selected. Otherwise revert
        -- to the first available match.
        let unames = matchFull <$> usernameMatches
            cnames = matchFull <$> chanMatches
            firstAvailableMatch =
                if null chanMatches
                then if null unames
                     then Nothing
                     else Just $ UserMatch $ head unames
                else Just $ ChannelMatch $ head cnames
            newMatch = case oldMatch of
              Just (UserMatch u) ->
                  if newInput `elem` unames
                  then Just $ UserMatch newInput
                  else if u `elem` unames
                       then oldMatch
                       else firstAvailableMatch
              Just (ChannelMatch c) ->
                  if newInput `elem` cnames
                  then Just $ ChannelMatch $ newInput
                  else if c `elem` cnames
                       then oldMatch
                       else firstAvailableMatch
              Nothing -> firstAvailableMatch
        in newMatch

applySelectPattern :: ChannelSelectPattern -> Text -> Maybe ChannelSelectMatch
applySelectPattern (CSP ty pat) chanName = do
    let applyType Infix  | pat `T.isInfixOf`  chanName =
            case T.breakOn pat chanName of
                (pre, post) -> return (pre, pat, T.drop (T.length pat) post)

        applyType Prefix | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType UsersOnly | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType ChannelsOnly | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType Suffix | pat `T.isSuffixOf` chanName = do
            let (b, a) = T.splitAt (T.length chanName - T.length pat) chanName
            return (b, a, "")

        applyType Equal  | pat == chanName =
            return ("", chanName, "")

        applyType _ = Nothing

    (pre, m, post) <- applyType ty
    return $ ChannelSelectMatch pre m post chanName

parseChannelSelectPattern :: Text -> Maybe ChannelSelectPattern
parseChannelSelectPattern pat = do
    let only = if | userSigil `T.isPrefixOf` pat -> Just $ CSP UsersOnly $ T.tail pat
                  | normalChannelSigil `T.isPrefixOf` pat -> Just $ CSP ChannelsOnly $ T.tail pat
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
-- determines how the new cursor position is computed from the old
-- one. The new cursor position is automatically wrapped around to the
-- beginning or end of the channel selection match list, so cursor
-- transformations do not have to do index validation. If the current
-- match (e.g. the sentinel "") is not found in the match list, this
-- sets the cursor position to the first match, if any.
updateSelectedMatch :: (Int -> Int) -> MH ()
updateSelectedMatch nextIndex = do
    chanMatches <- use (csChannelSelectState.channelMatches)
    usernameMatches <- use (csChannelSelectState.userMatches)

    csChannelSelectState.selectedMatch %= \oldMatch ->
        -- Make the list of all matches, in display order.
        let allMatches = concat [ (ChannelMatch . matchFull) <$> chanMatches
                                , (UserMatch . matchFull) <$> usernameMatches
                                ]
            defaultMatch = if null allMatches
                           then Nothing
                           else Just $ allMatches !! 0
        in case oldMatch of
            Nothing -> defaultMatch
            Just oldMatch' -> case findIndex (== oldMatch') allMatches of
                Nothing -> defaultMatch
                Just i ->
                    let newIndex = if tmpIndex < 0
                                   then length allMatches - 1
                                   else if tmpIndex >= length allMatches
                                        then 0
                                        else tmpIndex
                        tmpIndex = nextIndex i
                    in Just $ allMatches !! newIndex
