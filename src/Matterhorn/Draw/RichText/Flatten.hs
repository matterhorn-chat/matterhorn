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

import           Matterhorn.Constants ( normalChannelSigil, userSigil )
import           Matterhorn.Types ( HighlightSet(..) )
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
data FlattenedInline =
    FlattenedInline { fiValue :: FlattenedContent
                    -- ^ The content of the value.
                    , fiStyles :: [InlineStyle]
                    -- ^ The styles that should be applied to this
                    -- value.
                    , fiURL :: Maybe URL
                    -- ^ If present, the URL to which we should
                    -- hyperlink this value.
                    }

-- | A flattened value.
data FlattenedValue =
    SingleInline FlattenedInline
    -- ^ A single flattened value
    | NonBreaking (Seq (Seq FlattenedValue))
    -- ^ A sequence of flattened values that MUST be kept together and
    -- never broken up by line-wrapping

-- | The visual styles of inline values.
data InlineStyle =
    Strong
    | Emph
    | Strikethrough
    | Code
    | Permalink
    deriving (Eq, Show)

type FlattenM a = ReaderT FlattenEnv (State FlattenState) a

-- | The flatten monad state
data FlattenState =
    FlattenState { fsCompletedLines :: Seq (Seq FlattenedValue)
                 -- ^ The lines that we have accumulated so far in the
                 -- flattening process
                 , fsCurLine :: Seq FlattenedValue
                 -- ^ The current line we are accumulating in the
                 -- flattening process
                 }

-- | The flatten monad environment
data FlattenEnv =
    FlattenEnv { flattenStyles :: [InlineStyle]
               -- ^ The styles that should apply to the current value
               -- being flattened
               , flattenURL :: Maybe URL
               -- ^ The hyperlink URL, if any, that should be applied to
               -- the current value being flattened
               , flattenHighlightSet :: HighlightSet
               -- ^ The highlight set to use to check for valid user or
               -- channel references
               }

-- | Given a sequence of inlines, flatten it into a list of lines of
-- flattened values.
flattenInlineSeq :: HighlightSet -> Inlines -> Seq (Seq FlattenedValue)
flattenInlineSeq hs is =
    flattenInlineSeq' initialEnv is
    where
        initialEnv = FlattenEnv { flattenStyles = []
                                , flattenURL = Nothing
                                , flattenHighlightSet = hs
                                }

flattenInlineSeq' :: FlattenEnv -> Inlines -> Seq (Seq FlattenedValue)
flattenInlineSeq' env is =
    fsCompletedLines $ execState (runReaderT (mapM_ flatten (unInlines is) >> pushFLine) env) initialState
    where
        initialState = FlattenState mempty mempty

withInlineStyle :: InlineStyle -> FlattenM () -> FlattenM ()
withInlineStyle s = withReaderT (\e -> e { flattenStyles = nub (s : flattenStyles e) })

withHyperlink :: URL -> FlattenM () -> FlattenM ()
withHyperlink u = withReaderT (\e -> e { flattenURL = Just u })

-- | Push a FlattenedContent value onto the current line.
pushFC :: FlattenedContent -> FlattenM ()
pushFC v = do
    env <- ask
    let styles = flattenStyles env
        mUrl = flattenURL env
        fi = FlattenedInline { fiValue = v
                             , fiStyles = styles
                             , fiURL = mUrl
                             }
    pushFV $ SingleInline fi

-- | Push a FlattenedValue onto the current line.
pushFV :: FlattenedValue -> FlattenM ()
pushFV fv = lift $ modify $ \s -> s { fsCurLine = fsCurLine s |> fv }

-- | Push the current line onto the finished lines list and start a new
-- line.
pushFLine :: FlattenM ()
pushFLine =
    lift $ modify $ \s -> s { fsCompletedLines = fsCompletedLines s |> fsCurLine s
                            , fsCurLine = mempty
                            }

isKnownUser :: T.Text -> FlattenM Bool
isKnownUser u = do
    hSet <- asks flattenHighlightSet
    let uSet = hUserSet hSet
    return $ u `Set.member` uSet

isKnownChannel :: T.Text -> FlattenM Bool
isKnownChannel c = do
    hSet <- asks flattenHighlightSet
    let cSet = hChannelSet hSet
    return $ c `Set.member` cSet

flatten :: Inline -> FlattenM ()
flatten i =
    case i of
        EUser u -> do
            known <- isKnownUser u
            if known then pushFC (FUser u)
                     else pushFC (FText $ userSigil <> u)
        EChannel c -> do
            known <- isKnownChannel c
            if known then pushFC (FChannel c)
                     else pushFC (FText $ normalChannelSigil <> c)

        ENonBreaking is -> do
            env <- ask
            pushFV $ (NonBreaking $ flattenInlineSeq' env is)

        ESoftBreak                  -> pushFLine
        ELineBreak                  -> pushFLine

        EText t                     -> pushFC $ FText t
        ESpace                      -> pushFC FSpace
        ERawHtml h                  -> pushFC $ FText h
        EEmoji e                    -> pushFC $ FEmoji e
        EEditSentinel r             -> pushFC $ FEditSentinel r

        EEmph es                    -> withInlineStyle Emph $ mapM_ flatten $ unInlines es
        EStrikethrough es           -> withInlineStyle Strikethrough $ mapM_ flatten $ unInlines es
        EStrong es                  -> withInlineStyle Strong $ mapM_ flatten $ unInlines es
        ECode es                    -> withInlineStyle Code $ mapM_ flatten $ unInlines es

        EPermalink _ _ label@(Inlines ls) ->
            let label' = if Seq.null ls
                         then Inlines $ Seq.fromList [EText "<post", ESpace, EText "link>"]
                         else label
            in withInlineStyle Permalink $ mapM_ flatten $ unInlines $ decorateLinkLabel label'

        EHyperlink u label@(Inlines ls) ->
            let label' = if Seq.null ls
                         then Inlines $ Seq.singleton $ EText $ unURL u
                         else label
            in withHyperlink u $ mapM_ flatten $ unInlines $ decorateLinkLabel label'

        EImage u label@(Inlines ls) ->
            let label' = if Seq.null ls
                         then Inlines $ Seq.singleton $ EText $ unURL u
                         else label
            in withHyperlink u $ mapM_ flatten $ unInlines $ decorateLinkLabel label'

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
