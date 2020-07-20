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
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Data.Sequence ( (<|), ViewL((:<)) )
import qualified Data.Text as T

import           Types.UserNames ( isNameFragment )
import           Types ( userSigil, normalChannelSigil )

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

setStyle :: ElementStyle -> Element -> Element
setStyle sty e = e { eStyle = sty }

data ElementData =
    EText Text
    | ESpace
    | ESoftBreak
    | ELineBreak
    | ERawHtml Text
    | EEditSentinel
    | EEditRecentlySentinel
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
    Para $ fromMarkdownInlines is
fromMarkdownBlock (C.Header level is) =
    Header level $ fromMarkdownInlines is
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

fromMarkdownInlines :: Seq C.Inline -> Seq Element
fromMarkdownInlines inlines =
    let go sty is = case Seq.viewl is of
          C.Str t :< xs | t == editMarkingSentinel ->
              Element Edited EEditSentinel <| go sty xs
          C.Str t :< xs | t == editRecentlyMarkingSentinel ->
              Element EditedRecently EEditRecentlySentinel <| go sty xs
          C.Str t :< xs | userSigil `T.isPrefixOf` t ->
              let (uFrags, rest) = Seq.spanl isNameFragment xs
                  t' = T.concat $ t : (unsafeGetStr <$> F.toList uFrags)
                  u = T.drop 1 t'
              in Element sty (EUser u) <| go sty rest
          C.Str t :< xs | normalChannelSigil `T.isPrefixOf` t ->
              let (cFrags, rest) = Seq.spanl isNameFragment xs
                  cn = T.drop 1 $ T.concat $ t : (unsafeGetStr <$> F.toList cFrags)
              in Element sty (EChannel cn) <| go sty rest
          C.Str t :< xs ->
              Element sty (EText t) <| go sty xs
          C.Space :< xs ->
              Element sty ESpace <| go sty xs
          C.SoftBreak :< xs ->
              Element sty ESoftBreak <| go sty xs
          C.LineBreak :< xs ->
              Element sty ELineBreak <| go sty xs
          C.Link label url _ :< xs ->
              (setStyle (Link url) <$> fromMarkdownInlines label) <> go sty xs
          C.RawHtml t :< xs ->
              Element sty (ERawHtml t) <| go sty xs
          C.Code t :< xs ->
              -- We turn a single code string into individual Elements
              -- so we can perform line-wrapping on the inline code
              -- text.
              let ts = [ Element Code frag
                       | wd <- T.split (== ' ') t
                       , frag <- case wd of
                           "" -> [ESpace]
                           _  -> [ESpace, EText wd]
                       ]
                  ts' = case ts of
                    (Element _ ESpace:rs) -> rs
                    _                     -> ts
              in Seq.fromList ts' <> go sty xs
          C.Emph as :< xs ->
              go Emph as <> go sty xs
          C.Strong as :< xs ->
              go Strong as <> go sty xs
          C.Image altIs url _ :< xs ->
              (setStyle (Link url) <$> fromMarkdownInlines altIs) <> go sty xs
          C.Entity t :< xs ->
              Element sty (EText t) <| go sty xs
          Seq.EmptyL -> mempty

    in go Normal inlines

elementWidth :: Element -> Int
elementWidth e =
    case eData e of
        EText t               -> textWidth t
        ERawHtml t            -> textWidth t
        EUser t               -> textWidth t
        EChannel t            -> textWidth t
        EEditSentinel         -> textWidth editMarking
        EEditRecentlySentinel -> textWidth editMarking
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

unsafeGetStr :: C.Inline -> Text
unsafeGetStr (C.Str t) = t
unsafeGetStr _ = error "BUG: unsafeGetStr called on non-Str Inline"
