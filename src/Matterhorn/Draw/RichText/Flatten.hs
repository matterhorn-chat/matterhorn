-- | This module implements a "flattening" pass over RichText 'Inline'
-- values. This means that a tree structure such as
--
-- @
--   EStrong
--     [ EStrikethrough
--       [ EText "inside"
--       ]
--     , EText "outside"
--     ]
-- @
--
-- will be converted into a "flat" representation without a tree
-- structure so that the style information encoded in the tree is
-- available at each node:
--
-- @
--   [
--     [ SingleInline (FlattenedInline (FText "inside") [Strong, Strikethrough] Nothing
--     , SingleInline (FlattenedInline (FText "outside") [Strong] Nothing
--     ]
--   ]
-- @
--
-- The outer sequence is a sequence of lines (since inline lists can
-- introduce line breaks). Each inner sequence is a single line.
-- Each 'SingleInline' can be rendered as-is; if a 'NonBreaking' is
-- encountered, that group of inlines should be treated as a unit for
-- the purposes of line-wrapping (to happen in the Wrap module). The
-- above representation example shows how the tree path including the
-- 'EStrong' and 'EStrikethrough' nodes is flattened into a list of
-- styles to accompany each inline value. This makes it trivial to carry
-- that style information along with each node during line-wrapping
-- rather than needing to deal with the tree structure.
module Matterhorn.Draw.RichText.Flatten
  ( FlattenedContent(..)
  , FlattenedInline(..)
  , InlineStyle(..)
  , FlattenedValue(..)
  , flattenInlineSeq
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List ( nub )
import qualified Data.Sequence as Seq
import           Data.Sequence ( ViewL(..)
                               , ViewR(..)
                               , (<|)
                               , (|>)
                               )
import qualified Data.Set as Set
import qualified Data.Text as T

import           Matterhorn.Constants ( normalChannelSigil )
import           Matterhorn.Types ( HighlightSet(..), SemEq(..), addUserSigil )
import           Matterhorn.Types.RichText


-- | A piece of text in a sequence of flattened RichText elements. This
-- type represents the lowest-level kind of data that we can get from a
-- rich text document.
data FlattenedContent =
    FText Text
    -- ^ Some text
    | FSpace
    -- ^ A space
    | FUser Text
    -- ^ A user reference
    | FChannel Text
    -- ^ A channel reference
    | FEmoji Text
    -- ^ An emoji
    | FEditSentinel Bool
    -- ^ An "edited" marking
    deriving (Eq, Show)

-- | A flattened inline value.
data FlattenedInline a =
    FlattenedInline { fiValue :: FlattenedContent
                    -- ^ The content of the value.
                    , fiStyles :: [InlineStyle]
                    -- ^ The styles that should be applied to this
                    -- value.
                    , fiURL :: Maybe URL
                    -- ^ If present, the URL to which we should
                    -- hyperlink this value.
                    , fiName :: Maybe a
                    -- ^ The resource name, if any, that should be used
                    -- to make this inline clickable once rendered.
                    }
                    deriving (Show)

-- | A flattened value.
data FlattenedValue a =
    SingleInline (FlattenedInline a)
    -- ^ A single flattened value
    | NonBreaking (Seq (Seq (FlattenedValue a)))
    -- ^ A sequence of flattened values that MUST be kept together and
    -- never broken up by line-wrapping
    deriving (Show)

-- | The visual styles of inline values.
data InlineStyle =
    Strong
    | Emph
    | Strikethrough
    | Code
    | Permalink
    deriving (Eq, Show)

type FlattenM n a = ReaderT (FlattenEnv n) (State (FlattenState n)) a

-- | The flatten monad state
data FlattenState a =
    FlattenState { fsCompletedLines :: Seq (Seq (FlattenedValue a))
                 -- ^ The lines that we have accumulated so far in the
                 -- flattening process
                 , fsCurLine :: Seq (FlattenedValue a)
                 -- ^ The current line we are accumulating in the
                 -- flattening process
                 , fsNameIndex :: Int
                 -- ^ The index used to generate a new unique name (of
                 -- type 'a') to make a region of text clickable.
                 }

-- | The flatten monad environment
data FlattenEnv a =
    FlattenEnv { flattenStyles :: [InlineStyle]
               -- ^ The styles that should apply to the current value
               -- being flattened
               , flattenURL :: Maybe URL
               -- ^ The hyperlink URL, if any, that should be applied to
               -- the current value being flattened
               , flattenHighlightSet :: HighlightSet
               -- ^ The highlight set to use to check for valid user or
               -- channel references
               , flattenNameGen :: Maybe (Int -> Inline -> Maybe a)
               -- ^ The function to use to generate resource names
               -- for clickable inlines. If provided, this is used to
               -- determine whether a given Inline should be augmented
               -- with a resource name.
               , flattenNameFunc :: Maybe (Int -> Maybe a)
               -- ^ The currently active function to generate a resource
               -- name for any inline. In practice this is just the
               -- value of flattenNameGen, but partially applied with a
               -- specific Inline prior to flattening that Inline.
               }

-- | Given a sequence of inlines, flatten it into a list of lines of
-- flattened values.
--
-- The flattening process also validates user and channel references
-- against a 'HighlightSet'. For example, if an 'EUser' node is found,
-- its username argument is looked up in the 'HighlightSet'. If the
-- username is found, the 'EUser' node is preserved as an 'FUser' node.
-- Otherwise it is rewritten as an 'FText' node so that the username
-- does not get highlighted. Channel references ('EChannel') are handled
-- similarly.
--
-- The optional name generator function argument is used to assign
-- resource names to each inline that should be clickable once rendered.
-- The result of the name generator function will be stored in the
-- 'fiName' field of each 'FlattenedInline' that results from calling
-- that function on an 'Inline'.
flattenInlineSeq :: SemEq a
                 => HighlightSet
                 -> Maybe (Int -> Inline -> Maybe a)
                 -- ^ A name generator function for clickable inlines.
                 -- The integer argument is a unique (to this inline
                 -- sequence) sequence number.
                 -> Inlines
                 -> Seq (Seq (FlattenedValue a))
flattenInlineSeq hs nameGen is =
    snd $ flattenInlineSeq' initialEnv 0 is
    where
        initialEnv = FlattenEnv { flattenStyles = []
                                , flattenURL = Nothing
                                , flattenHighlightSet = hs
                                , flattenNameGen = nameGen
                                , flattenNameFunc = Nothing
                                }

flattenInlineSeq' :: SemEq a
                  => FlattenEnv a
                  -> Int
                  -> Inlines
                  -> (Int, Seq (Seq (FlattenedValue a)))
flattenInlineSeq' env c is =
    (fsNameIndex finalState, fsCompletedLines finalState)
    where
        finalState = execState stBody initialState
        initialState = FlattenState { fsCompletedLines = mempty
                                    , fsCurLine = mempty
                                    , fsNameIndex = c
                                    }
        stBody = runReaderT body env
        body = do
            flattenInlines is
            pushFLine

flattenInlines :: SemEq a => Inlines -> FlattenM a ()
flattenInlines is = do
    pairs <- nameInlinePairs
    mapM_ wrapFlatten pairs
    where
        wrapFlatten (nameFunc, i) = withNameFunc nameFunc $ flatten i

        -- For each inline, prior to flattening it, obtain the resource
        -- name (if any) that should be assigned to each flattened
        -- fragment of the inline.
        nameInlinePairs = forM (unInlines is) $ \i -> do
            nameFunc <- nameGenWrapper i
            return (nameFunc, i)

        -- Determine whether the name generation function will produce
        -- a name for this inline. If it does (using a fake sequence
        -- number) then return a new name generation function to use for
        -- all flattened fragments of this inline.
        nameGenWrapper :: Inline -> FlattenM a (Maybe (Int -> Maybe a))
        nameGenWrapper i = do
            c <- gets fsNameIndex
            nameGen <- asks flattenNameGen
            return $ case nameGen of
                Nothing -> Nothing
                Just f -> if isJust (f c i) then Just (flip f i) else Nothing

withNameFunc :: Maybe (Int -> Maybe a) -> FlattenM a () -> FlattenM a ()
withNameFunc f@(Just _) = withReaderT (\e -> e { flattenNameFunc = f })
withNameFunc Nothing = id

withInlineStyle :: InlineStyle -> FlattenM a () -> FlattenM a ()
withInlineStyle s =
    withReaderT (\e -> e { flattenStyles = nub (s : flattenStyles e) })

withHyperlink :: URL -> FlattenM a () -> FlattenM a ()
withHyperlink u = withReaderT (\e -> e { flattenURL = Just u })

-- | Push a FlattenedContent value onto the current line.
pushFC :: SemEq a => FlattenedContent -> FlattenM a ()
pushFC v = do
    env <- ask
    name <- getNextName
    let styles = flattenStyles env
        mUrl = flattenURL env
        fi = FlattenedInline { fiValue = v
                             , fiStyles = styles
                             , fiURL = mUrl
                             , fiName = name
                             }
    pushFV $ SingleInline fi

getNextName :: FlattenM a (Maybe a)
getNextName = do
    nameGen <- asks flattenNameFunc
    case nameGen of
        Nothing -> return Nothing
        Just f -> f <$> getNextNameIndex

getNextNameIndex :: FlattenM a Int
getNextNameIndex = do
    c <- gets fsNameIndex
    modify ( \s -> s { fsNameIndex = c + 1} )
    return c

setNextNameIndex :: Int -> FlattenM a ()
setNextNameIndex i = modify ( \s -> s { fsNameIndex = i } )

-- | Push a FlattenedValue onto the current line.
pushFV :: SemEq a => FlattenedValue a -> FlattenM a ()
pushFV fv = lift $ modify $ \s -> s { fsCurLine = appendFV fv (fsCurLine s) }

-- | Append the value to the sequence.
--
-- If the both the value to append AND the sequence's last value are
-- both text nodes, AND if those nodes both have the same style and URL
-- metadata, then merge them into one text node. This keeps adjacent
-- non-whitespace text together as one logical token (e.g. "(foo" rather
-- than "(" followed by "foo") to avoid undesirable line break points in
-- the wrapping process.
appendFV :: SemEq a => FlattenedValue a -> Seq (FlattenedValue a) -> Seq (FlattenedValue a)
appendFV v line =
    case (Seq.viewr line, v) of
        (h :> SingleInline a, SingleInline b) ->
            case (fiValue a, fiValue b) of
                (FText aT, FText bT) ->
                    if fiStyles a == fiStyles b && fiURL a == fiURL b && fiName a `semeq` fiName b
                    then h |> SingleInline (FlattenedInline (FText $ aT <> bT)
                                                            (fiStyles a)
                                                            (fiURL a)
                                                            (max (fiName a) (fiName b)))
                    else line |> v
                _ -> line |> v
        _ -> line |> v

-- | Push the current line onto the finished lines list and start a new
-- line.
pushFLine :: FlattenM a ()
pushFLine =
    lift $ modify $ \s -> s { fsCompletedLines = fsCompletedLines s |> fsCurLine s
                            , fsCurLine = mempty
                            }

isKnownUser :: T.Text -> FlattenM a Bool
isKnownUser u = do
    hSet <- asks flattenHighlightSet
    let uSet = hUserSet hSet
    return $ u `Set.member` uSet

isKnownChannel :: T.Text -> FlattenM a Bool
isKnownChannel c = do
    hSet <- asks flattenHighlightSet
    let cSet = hChannelSet hSet
    return $ c `Set.member` cSet

flatten :: SemEq a => Inline -> FlattenM a ()
flatten i =
    case i of
        EUser u -> do
            known <- isKnownUser u
            if known then pushFC (FUser u)
                     else pushFC (FText $ addUserSigil u)
        EChannel c -> do
            known <- isKnownChannel c
            if known then pushFC (FChannel c)
                     else pushFC (FText $ normalChannelSigil <> c)

        ENonBreaking is -> do
            env <- ask
            ni <- getNextNameIndex
            let (ni', s) = flattenInlineSeq' env ni is
            pushFV $ NonBreaking s
            setNextNameIndex ni'

        ESoftBreak                  -> pushFLine
        ELineBreak                  -> pushFLine

        EText t                     -> pushFC $ FText t
        ESpace                      -> pushFC FSpace
        ERawHtml h                  -> pushFC $ FText h
        EEmoji e                    -> pushFC $ FEmoji e
        EEditSentinel r             -> pushFC $ FEditSentinel r

        EEmph es                    -> withInlineStyle Emph $ flattenInlines es
        EStrikethrough es           -> withInlineStyle Strikethrough $ flattenInlines es
        EStrong es                  -> withInlineStyle Strong $ flattenInlines es
        ECode es                    -> withInlineStyle Code $ flattenInlines es

        EPermalink _ _ mLabel ->
            let label' = fromMaybe (Inlines $ Seq.fromList [EText "post", ESpace, EText "link"])
                                   mLabel
            in withInlineStyle Permalink $ flattenInlines $ decorateLinkLabel label'

        EHyperlink u label@(Inlines ls) ->
            let label' = if Seq.null ls
                         then Inlines $ Seq.singleton $ EText $ unURL u
                         else label
            in withHyperlink u $ flattenInlines $ decorateLinkLabel label'

        EImage u label@(Inlines ls) ->
            let label' = if Seq.null ls
                         then Inlines $ Seq.singleton $ EText $ unURL u
                         else label
            in withHyperlink u $ flattenInlines $ decorateLinkLabel label'

linkOpenBracket :: Inline
linkOpenBracket = EText "<"

linkCloseBracket :: Inline
linkCloseBracket = EText ">"

addOpenBracket :: Inlines -> Inlines
addOpenBracket (Inlines l) =
    case Seq.viewl l of
        EmptyL -> Inlines l
        h :< t ->
            let h' = ENonBreaking $ Inlines $ Seq.fromList [linkOpenBracket, h]
            in Inlines $ h' <| t

addCloseBracket :: Inlines -> Inlines
addCloseBracket (Inlines l) =
    case Seq.viewr l of
        EmptyR -> Inlines l
        h :> t ->
            let t' = ENonBreaking $ Inlines $ Seq.fromList [t, linkCloseBracket]
            in Inlines $ h |> t'

decorateLinkLabel :: Inlines -> Inlines
decorateLinkLabel = addOpenBracket .  addCloseBracket
