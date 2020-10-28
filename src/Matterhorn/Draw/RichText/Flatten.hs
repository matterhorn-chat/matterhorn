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


data FlattenedContent =
    FText Text
    | FSpace
    | FUser Text
    | FChannel Text
    | FEmoji Text
    | FEditSentinel Bool
    deriving (Eq, Show)

data FlattenedInline =
    FlattenedInline { fiValue :: FlattenedContent
                    , fiStyles :: [InlineStyle]
                    , fiURL :: Maybe URL
                    }

data FlattenedValue =
    SingleInline FlattenedInline
    | NonBreaking (Seq (Seq FlattenedValue))

data InlineStyle =
    Strong
    | Emph
    | Strikethrough
    | Code
    | Permalink
    deriving (Eq, Show)

type FlattenM a = ReaderT FlattenEnv (State FlattenState) a

data FlattenState =
    FlattenState { fsCompletedLines :: Seq (Seq FlattenedValue)
                 , fsCurLine :: Seq FlattenedValue
                 }

data FlattenEnv =
    FlattenEnv { flattenStyles :: [InlineStyle]
               , flattenURL :: Maybe URL
               , flattenHighlightSet :: HighlightSet
               }

flattenInlineSeq :: HighlightSet -> Seq Inline -> Seq (Seq FlattenedValue)
flattenInlineSeq hs is =
    flattenInlineSeq' initialEnv is
    where
        initialEnv = FlattenEnv { flattenStyles = []
                                , flattenURL = Nothing
                                , flattenHighlightSet = hs
                                }

flattenInlineSeq' :: FlattenEnv -> Seq Inline -> Seq (Seq FlattenedValue)
flattenInlineSeq' env is =
    fsCompletedLines $ execState (runReaderT (mapM_ flatten is >> pushFLine) env) initialState
    where
        initialState = FlattenState mempty mempty

withInlineStyle :: InlineStyle -> FlattenM () -> FlattenM ()
withInlineStyle s = withReaderT (\e -> e { flattenStyles = nub (s : flattenStyles e) })

withHyperlink :: URL -> FlattenM () -> FlattenM ()
withHyperlink u = withReaderT (\e -> e { flattenURL = Just u })

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

pushFV :: FlattenedValue -> FlattenM ()
pushFV fv = lift $ modify $ \s -> s { fsCurLine = fsCurLine s |> fv }

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

        EEmph es                    -> withInlineStyle Emph $ mapM_ flatten es
        EStrikethrough es           -> withInlineStyle Strikethrough $ mapM_ flatten es
        EStrong es                  -> withInlineStyle Strong $ mapM_ flatten es
        ECode es                    -> withInlineStyle Code $ mapM_ flatten es
        EPermalink _ _ Nothing      -> withInlineStyle Permalink $ mapM_ flatten $ Seq.fromList [EText "<post", ESpace, EText "link>"]
        EPermalink _ _ (Just label) -> withInlineStyle Permalink $ mapM_ flatten $ decorateLinkLabel label

        EHyperlink u Nothing        -> withHyperlink u $ mapM_ flatten (decorateLinkLabel $ Seq.singleton $ EText $ unURL u)
        EHyperlink u (Just label)   -> withHyperlink u $ mapM_ flatten $ decorateLinkLabel label
        EImage u Nothing            -> withHyperlink u $ mapM_ flatten (decorateLinkLabel $ Seq.singleton $ EText $ unURL u)
        EImage u (Just label)       -> withHyperlink u $ mapM_ flatten $ decorateLinkLabel label

linkOpenBracket :: Inline
linkOpenBracket = EText "<"

linkCloseBracket :: Inline
linkCloseBracket = EText ">"

addOpenBracket :: Seq Inline -> Seq Inline
addOpenBracket l =
    case Seq.viewl l of
        EmptyL -> l
        h :< t ->
            let h' = ENonBreaking $ Seq.fromList [linkOpenBracket, h]
            in h' <| t

addCloseBracket :: Seq Inline -> Seq Inline
addCloseBracket l =
    case Seq.viewr l of
        EmptyR -> l
        h :> t ->
            let t' = ENonBreaking $ Seq.fromList [t, linkCloseBracket]
            in h |> t'

decorateLinkLabel :: Seq Inline -> Seq Inline
decorateLinkLabel = addOpenBracket .  addCloseBracket
