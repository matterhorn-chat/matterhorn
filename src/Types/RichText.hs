module Types.RichText
  ( RichText(..)
  , RichTextBlock(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Element(..)
  , ElementStyle(..)

  , fromMarkdownBlocks
  )
where

import           Prelude ()
import           Prelude.MH

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
    EText Text
    | ESpace
    | ESoftBreak
    | ELineBreak
    | ELink (Seq Element) Text
    | EEntity Text
    | ERawHtml Text
    | EEditSentinel
    | EEditRecentlySentinel
    | ESequence (Seq Element)
    | EUser Text
    | EChannel Text
    | EStyled ElementStyle Element
    deriving (Show, Eq)

data ElementStyle =
    Normal
    | Emph
    | Strong
    | Code
    | Edited
    | EditedRecently
    deriving (Eq, Show)

fromMarkdownBlocks :: C.Blocks -> RichText
fromMarkdownBlocks bs =
    RichText $ fromMarkdownBlock <$> bs

fromMarkdownBlock :: C.Block -> RichTextBlock
fromMarkdownBlock (C.Para is) =
    Para $ fromMarkdownInline <$> is
fromMarkdownBlock (C.Header level is) =
    Header level $ fromMarkdownInline <$> is
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

fromMarkdownInline :: C.Inline -> Element
fromMarkdownInline C.Space =
    ESpace
fromMarkdownInline C.SoftBreak =
    ESoftBreak
fromMarkdownInline C.LineBreak =
    ELineBreak
fromMarkdownInline (C.Str t) =
    EText t
fromMarkdownInline (C.Emph is) =
    EStyled Emph $ ESequence $ fromMarkdownInline <$> is
fromMarkdownInline (C.Strong is) =
    EStyled Strong $ ESequence $ fromMarkdownInline <$> is
fromMarkdownInline (C.Code t) =
    EStyled Code $ EText t
fromMarkdownInline (C.Link labelIs url _) =
    ELink (fromMarkdownInline <$> labelIs) url
fromMarkdownInline (C.Image altIs url _) =
    ELink (fromMarkdownInline <$> altIs) url
fromMarkdownInline (C.Entity t) =
    EEntity t
fromMarkdownInline (C.RawHtml body) =
    ERawHtml body
