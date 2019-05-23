module State.Autocomplete
  ( AutocompleteContext(..)
  , checkForAutocompletion
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning )
import           Brick.Widgets.Edit ( editContentsL )
import qualified Brick.Widgets.List as L
import           Data.Char ( isSpace )
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List ( sortBy, partition )
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (%=), (.=), (.~), _Just, preuse )
import qualified Skylighting.Types as Sky

import           Network.Mattermost.Types (userId, channelId)
import qualified Network.Mattermost.Endpoints as MM

import {-# SOURCE #-} Command ( commandList, printArgSpec )
import           State.Common
import {-# SOURCE #-} State.Editing ( Direction(..), tabComplete )
import           Types hiding ( newState )
import           Emoji


data AutocompleteContext =
    AutocompleteContext { autocompleteManual :: Bool
                        -- ^ Whether the autocompletion was manual
                        -- (True) or automatic (False). The automatic
                        -- case is the case where the autocomplete
                        -- lookups and UI are triggered merely by
                        -- entering some initial text (such as "@").
                        -- The manual case is the case where the
                        -- autocomplete lookups and UI are triggered
                        -- explicitly by a user's TAB keypress.
                        }

-- | Check for whether the currently-edited word in the message editor
-- should cause an autocompletion UI to appear. If so, initiate a server
-- query or local cache lookup to present the completion alternatives
-- for the word at the cursor.
checkForAutocompletion :: AutocompleteContext -> MH ()
checkForAutocompletion ctx = do
    result <- getCompleterForInput ctx
    case result of
        Nothing -> resetAutocomplete
        Just (runUpdater, searchString) -> do
            prevResult <- use (csEditState.cedAutocomplete)
            let shouldUpdate = maybe True ((/= searchString) . _acPreviousSearchString)
                               prevResult
            when shouldUpdate $ do
                csEditState.cedAutocompletePending .= Just searchString
                runUpdater searchString

getCompleterForInput :: AutocompleteContext -> MH (Maybe (Text -> MH (), Text))
getCompleterForInput ctx = do
    z <- use (csEditState.cedEditor.editContentsL)

    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z

    return $ case wordAtColumn col curLine of
        Just (startCol, w)
            | userSigil `T.isPrefixOf` w ->
                Just (doUserAutoCompletion, T.tail w)
            | normalChannelSigil `T.isPrefixOf` w ->
                Just (doChannelAutoCompletion, T.tail w)
            | ":" `T.isPrefixOf` w && autocompleteManual ctx ->
                Just (doEmojiAutoCompletion, T.tail w)
            | "```" `T.isPrefixOf` w ->
                Just (doSyntaxAutoCompletion, T.drop 3 w)
            | "/" `T.isPrefixOf` w && startCol == 0 ->
                Just (doCommandAutoCompletion, T.tail w)
        _ -> Nothing

doEmojiAutoCompletion :: Text -> MH ()
doEmojiAutoCompletion searchString = do
    session <- getSession
    em <- use (csResources.crEmoji)
    let label = "Emoji"
    withCachedAutocompleteResults label searchString $
        doAsyncWith Preempt $ do
            results <- getMatchingEmoji session em searchString
            let alts = EmojiCompletion <$> results
            return $ Just $ setCompletionAlternatives searchString alts label

doSyntaxAutoCompletion :: Text -> MH ()
doSyntaxAutoCompletion searchString = do
    mapping <- use (csResources.crSyntaxMap)
    let allNames = Sky.sShortname <$> M.elems mapping
        (prefixed, notPrefixed) = partition isPrefixed $ filter match allNames
        match = (((T.toLower searchString) `T.isInfixOf`) . T.toLower)
        isPrefixed = (((T.toLower searchString) `T.isPrefixOf`) . T.toLower)
        alts = SyntaxCompletion <$> (sort prefixed <> sort notPrefixed)
    setCompletionAlternatives searchString alts "Languages"

doCommandAutoCompletion :: Text -> MH ()
doCommandAutoCompletion searchString = do
    let alts = mkAlt <$> sortBy compareCommands (filter matches commandList)
        compareCommands a b =
            let isAPrefix = searchString `T.isPrefixOf` cmdName a
                isBPrefix = searchString `T.isPrefixOf` cmdName b
            in if isAPrefix && isBPrefix
               then compare (cmdName a) (cmdName b)
               else if isAPrefix
                    then LT
                    else GT
        lowerSearch = T.toLower searchString
        matches c = lowerSearch `T.isInfixOf` (cmdName c) ||
                    lowerSearch `T.isInfixOf` (T.toLower $ cmdDescr c)
        mkAlt (Cmd name desc args _) =
            CommandCompletion name (printArgSpec args) desc
    setCompletionAlternatives searchString alts "Commands"

-- | Attempt to re-use a cached autocomplete alternative list for
-- a given search string. If the cache contains no such entry (keyed
-- on search string), run the specified action, which is assumed to be
-- responsible for fetching the completion results from the server.
withCachedAutocompleteResults :: Text
                              -- ^ The autocomplete UI label for the
                              -- results to be used
                              -> Text
                              -- ^ The search string to look for in the
                              -- cache
                              -> MH ()
                              -- ^ The action to execute on a cache miss
                              -> MH ()
withCachedAutocompleteResults label searchString act = do
    mCache <- preuse (csEditState.cedAutocomplete._Just.acCachedResponses)

    -- Does the cache have results for this search string? If so, use
    -- them; otherwise invoke the specified action.
    case HM.lookup searchString =<< mCache of
        Just alts -> setCompletionAlternatives searchString alts label
        Nothing -> act

doUserAutoCompletion :: Text -> MH ()
doUserAutoCompletion searchString = do
    session <- getSession
    myTid <- gets myTeamId
    myUid <- gets myUserId
    cId <- use csCurrentChannelId
    let label = "Users"

    withCachedAutocompleteResults label searchString $
        doAsyncWith Preempt $ do
            ac <- MM.mmAutocompleteUsers (Just myTid) (Just cId) searchString session

            let active = Seq.filter (\u -> userId u /= myUid && (not $ userDeleted u))
                alts = F.toList $
                       ((\u -> UserCompletion u True) <$> (active $ MM.userAutocompleteUsers ac)) <>
                       (maybe mempty (fmap (\u -> UserCompletion u False) . active) $
                              MM.userAutocompleteOutOfChannel ac)

                specials = [ MentionAll
                           , MentionChannel
                           ]
                extras = [ SpecialMention m | m <- specials
                         , (T.toLower searchString) `T.isInfixOf` specialMentionName m
                         ]

            return $ Just $ setCompletionAlternatives searchString (extras <> alts) label

doChannelAutoCompletion :: Text -> MH ()
doChannelAutoCompletion searchString = do
    session <- getSession
    tId <- gets myTeamId
    let label = "Channels"
    cs <- use csChannels

    withCachedAutocompleteResults label searchString $ do
        doAsyncWith Preempt $ do
            results <- MM.mmAutocompleteChannels tId searchString session
            let alts = F.toList $ (ChannelCompletion True <$> inChannels) <>
                                  (ChannelCompletion False <$> notInChannels)
                (inChannels, notInChannels) = Seq.partition isMember results
                isMember c = isJust $ findChannelById (channelId c) cs
            return $ Just $ setCompletionAlternatives searchString alts label

setCompletionAlternatives :: Text -> [AutocompleteAlternative] -> Text -> MH ()
setCompletionAlternatives searchString alts ty = do
    let list = L.list CompletionList (V.fromList $ F.toList alts) 1
        state = AutocompleteState { _acPreviousSearchString = searchString
                                  , _acCompletionList =
                                      list & L.listSelectedL .~ Nothing
                                  , _acListElementType = ty
                                  , _acCachedResponses = HM.fromList [(searchString, alts)]
                                  }

    pending <- use (csEditState.cedAutocompletePending)
    case pending of
        Just val | val == searchString -> do

            -- If there is already state, update it, but also cache the
            -- search results.
            csEditState.cedAutocomplete %= \prev ->
                let newState = case prev of
                        Nothing ->
                            state
                        Just oldState ->
                            state & acCachedResponses .~
                                HM.insert searchString alts (oldState^.acCachedResponses)
                in Just newState

            mh $ vScrollToBeginning $ viewportScroll CompletionList
        _ ->
            -- Do not update the state if this result does not
            -- correspond to the search string we used most recently.
            -- This happens when the editor changes faster than the
            -- async completion responses arrive from the server. If we
            -- don't check this, we show completion results that are
            -- wrong for the editor state.
            return ()

wordAtColumn :: Int -> Text -> Maybe (Int, Text)
wordAtColumn i t =
    let tokens = T.groupBy (\a b -> isSpace a == isSpace b) t
        go _ j _ | j < 0 = Nothing
        go col j ts = case ts of
            [] -> Nothing
            (w:rest) | j <= T.length w && not (isSpace $ T.head w) -> Just (col, w)
                     | otherwise -> go (col + T.length w) (j - T.length w) rest
    in go 0 i tokens
