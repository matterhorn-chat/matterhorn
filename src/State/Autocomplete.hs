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
                        , autocompleteFirstMatch :: Bool
                        -- ^ Once the results of the autocomplete lookup
                        -- are available, this flag determines whether
                        -- the user's input is replaced immediately
                        -- with the first available match (True) or not
                        -- (False).
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
                runUpdater ctx searchString

getCompleterForInput :: AutocompleteContext -> MH (Maybe (AutocompleteContext -> Text -> MH (), Text))
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

doEmojiAutoCompletion :: AutocompleteContext -> Text -> MH ()
doEmojiAutoCompletion ctx searchString = do
    session <- getSession
    em <- use (csResources.crEmoji)
    let label = "Emoji"
        ty = ACEmoji
    withCachedAutocompleteResults ctx ty label searchString $
        doAsyncWith Preempt $ do
            results <- getMatchingEmoji session em searchString
            let alts = EmojiCompletion <$> results
            return $ Just $ setCompletionAlternatives ctx searchString alts ty label

doSyntaxAutoCompletion :: AutocompleteContext -> Text -> MH ()
doSyntaxAutoCompletion ctx searchString = do
    mapping <- use (csResources.crSyntaxMap)
    let allNames = Sky.sShortname <$> M.elems mapping
        (prefixed, notPrefixed) = partition isPrefixed $ filter match allNames
        match = (((T.toLower searchString) `T.isInfixOf`) . T.toLower)
        isPrefixed = (((T.toLower searchString) `T.isPrefixOf`) . T.toLower)
        alts = SyntaxCompletion <$> (sort prefixed <> sort notPrefixed)
    setCompletionAlternatives ctx searchString alts ACCodeBlockLanguage "Languages"

doCommandAutoCompletion :: AutocompleteContext -> Text -> MH ()
doCommandAutoCompletion ctx searchString = do
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
    setCompletionAlternatives ctx searchString alts ACCommands "Commands"

-- | Attempt to re-use a cached autocomplete alternative list for
-- a given search string. If the cache contains no such entry (keyed
-- on search string), run the specified action, which is assumed to be
-- responsible for fetching the completion results from the server.
withCachedAutocompleteResults :: AutocompleteContext
                              -- ^ The autocomplete context
                              -> AutocompletionType
                              -- ^ The type of autocompletion we're
                              -- doing
                              -> Text
                              -- ^ The autocomplete UI label for the
                              -- results to be used
                              -> Text
                              -- ^ The search string to look for in the
                              -- cache
                              -> MH ()
                              -- ^ The action to execute on a cache miss
                              -> MH ()
withCachedAutocompleteResults ctx ty label searchString act = do
    mCache <- preuse (csEditState.cedAutocomplete._Just.acCachedResponses)
    mActiveTy <- preuse (csEditState.cedAutocomplete._Just.acType)

    case Just ty == mActiveTy of
        True ->
            -- Does the cache have results for this search string? If
            -- so, use them; otherwise invoke the specified action.
            case HM.lookup searchString =<< mCache of
                Just alts -> setCompletionAlternatives ctx searchString alts ty label
                Nothing -> act
        False -> act

doUserAutoCompletion :: AutocompleteContext -> Text -> MH ()
doUserAutoCompletion ctx searchString = do
    session <- getSession
    myTid <- gets myTeamId
    myUid <- gets myUserId
    cId <- use csCurrentChannelId
    let label = "Users"
        ty = ACUsers

    withCachedAutocompleteResults ctx ty label searchString $
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

            return $ Just $ setCompletionAlternatives ctx searchString (extras <> alts) ty label

doChannelAutoCompletion :: AutocompleteContext -> Text -> MH ()
doChannelAutoCompletion ctx searchString = do
    session <- getSession
    tId <- gets myTeamId
    let label = "Channels"
        ty = ACChannels
    cs <- use csChannels

    withCachedAutocompleteResults ctx ty label searchString $ do
        doAsyncWith Preempt $ do
            results <- MM.mmAutocompleteChannels tId searchString session
            let alts = F.toList $ (ChannelCompletion True <$> inChannels) <>
                                  (ChannelCompletion False <$> notInChannels)
                (inChannels, notInChannels) = Seq.partition isMember results
                isMember c = isJust $ findChannelById (channelId c) cs
            return $ Just $ setCompletionAlternatives ctx searchString alts ty label

setCompletionAlternatives :: AutocompleteContext -> Text -> [AutocompleteAlternative] -> AutocompletionType -> Text -> MH ()
setCompletionAlternatives ctx searchString alts ty tyLabel = do
    let list = L.list CompletionList (V.fromList $ F.toList alts) 1
        state = AutocompleteState { _acPreviousSearchString = searchString
                                  , _acCompletionList =
                                      list & L.listSelectedL .~ Nothing
                                  , _acListElementType = tyLabel
                                  , _acCachedResponses = HM.fromList [(searchString, alts)]
                                  , _acType = ty
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

            when (autocompleteFirstMatch ctx) $
                tabComplete Forwards
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
