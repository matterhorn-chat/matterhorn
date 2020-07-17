module Types.RichText
  ( RichText(..)
  , RichTextBlock(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Element(..)
  , ElementData(..)
  , ElementStyle(..)

  , fromMarkdownBlocks
  , elementWidth
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( textWidth )
import qualified Cheapskate as C
import qualified Data.Sequence as Seq


data RichText =
    RichText { richTextBlocks :: Seq RichTextBlock
             }
             deriving (Show)

data RichTextBlock =
    Para (Seq Element)
    | Header Int (Seq Element)
    | Blockquote (Seq RichTextBlock)
    | List Bool ListType (Seq (Seq RichTextBlock))
    | CodeBlock CodeBlockInfo Text
    | HTMLBlock Text
    | HRule
    deriving (Show)

data ListType =
    Bullet Char
    | Numbered NumDecoration Int
    deriving (Eq, Show)

data CodeBlockInfo =
    CodeBlockInfo { codeBlockLanguage :: Text
                  , codeBlockInfo :: Text
                  }
                  deriving (Eq, Show)

data NumDecoration =
    Paren
    | Period
    deriving (Eq, Show)

data Element =
    Element { eStyle :: ElementStyle
            , eData :: ElementData
            }
            deriving (Show)

data ElementData =
    EText Text
    | ESpace
    | ESoftBreak
    | ELineBreak
    | ERawHtml Text
    | EEditSentinel
    | EEditRecentlySentinel
    -- TODO: do we even need this? It seems like places where we need
    -- sequences of elements already use the 'Seq Element' type.
    | ESequence (Seq Element)
    | EUser Text
    | EChannel Text
    deriving (Show)

data ElementStyle =
    Normal
    | Emph
    | Strong
    | Code
    | Edited
    | EditedRecently
    | Link Text
    -- ^ URL
    deriving (Eq, Show)

fromMarkdownBlocks :: C.Blocks -> RichText
fromMarkdownBlocks bs =
    RichText $ fromMarkdownBlock <$> bs

fromMarkdownBlock :: C.Block -> RichTextBlock
fromMarkdownBlock (C.Para is) =
    Para $ seqConcat $ fromMarkdownInline Normal <$> is
fromMarkdownBlock (C.Header level is) =
    Header level $ seqConcat $ fromMarkdownInline Normal <$> is
fromMarkdownBlock (C.Blockquote bs) =
    Blockquote $ fromMarkdownBlock <$> bs
fromMarkdownBlock (C.List f ty bss) =
    List f (fromMarkdownListType ty) $ fmap fromMarkdownBlock <$> Seq.fromList bss
fromMarkdownBlock (C.CodeBlock attr body) =
    CodeBlock (fromMarkdownCodeAttr attr) body
fromMarkdownBlock (C.HtmlBlock body) =
    HTMLBlock body
fromMarkdownBlock C.HRule =
    HRule

fromMarkdownCodeAttr :: C.CodeAttr -> CodeBlockInfo
fromMarkdownCodeAttr (C.CodeAttr lang info) =
    CodeBlockInfo lang info

fromMarkdownListType :: C.ListType -> ListType
fromMarkdownListType (C.Bullet c) =
    Bullet c
fromMarkdownListType (C.Numbered wrap i) =
    let dec = case wrap of
                  C.PeriodFollowing -> Period
                  C.ParenFollowing -> Paren
    in Numbered dec i

fromMarkdownInline :: ElementStyle -> C.Inline -> Seq Element
fromMarkdownInline _ (C.Emph is) =
    seqConcat $ fromMarkdownInline Emph <$> is
fromMarkdownInline _ (C.Strong is) =
    seqConcat $ fromMarkdownInline Strong <$> is
fromMarkdownInline s C.Space =
    Seq.singleton $ Element s ESpace
fromMarkdownInline s C.SoftBreak =
    Seq.singleton $ Element s ESoftBreak
fromMarkdownInline s C.LineBreak =
    Seq.singleton $ Element s ELineBreak
fromMarkdownInline s (C.Str t) =
    Seq.singleton $ Element s (EText t)
fromMarkdownInline _ (C.Code t) =
    Seq.singleton $ Element Code $ EText t
fromMarkdownInline s (C.Entity t) =
    Seq.singleton $ Element s $ EText t
fromMarkdownInline s (C.RawHtml body) =
    Seq.singleton $ Element s $ ERawHtml body
fromMarkdownInline _ (C.Link labelIs url _) =
    seqConcat $ fromMarkdownInline (Link url) <$> labelIs
fromMarkdownInline _ (C.Image altIs url _) =
    seqConcat $ fromMarkdownInline (Link url) <$> altIs

elementWidth :: Element -> Int
elementWidth e =
    case eData e of
        EText t               -> textWidth t
        ERawHtml t            -> textWidth t
        EUser t               -> textWidth t
        EChannel t            -> textWidth t
        EEditSentinel         -> textWidth editMarking
        EEditRecentlySentinel -> textWidth editMarking
        ESequence es          -> sum $ elementWidth <$> es
        ESpace                -> 1
        ELineBreak            -> 0
        ESoftBreak            -> 0

editMarking :: Text
editMarking = "(edited)"

-- The special string we use to indicate the placement of a styled
-- indication that a message has been edited.
editMarkingSentinel :: Text
editMarkingSentinel = "#__mh_edit"

-- The special string we use to indicate the placement of a styled
-- indication that a message has been edited recently.
editRecentlyMarkingSentinel :: Text
editRecentlyMarkingSentinel = "#__mh_edit_r"

seqConcat :: Seq (Seq a) -> Seq a
seqConcat ss = Seq.foldrWithIndex (\_ s rest -> s Seq.>< rest) mempty ss
