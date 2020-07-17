module Types.RichText
  ( RichText(..)
  , RichTextBlock(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Element(..)
  , ElementStyle(..)
  , ElementData(..)
  )
where

import           Prelude ()
import           Prelude.MH


data RichText =
    RichText { richTextBlocks :: Seq RichTextBlock
             }
             deriving (Show)

data RichTextBlock =
    Para (Seq Element)
    | Header Int (Seq Element)
    | Blockquote (Seq Element)
    | List Bool ListType (Seq RichTextBlock)
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
    Element { eData  :: ElementData
            , eStyle :: ElementStyle
            }
            deriving (Show, Eq)

data ElementData =
    EText Text
    | ESpace
    | ESoftBreak
    | ELineBreak
    | ELink Text Text
    | ERawHtml Text
    | EEditSentinel
    | EEditRecentlySentinel
    | ESequence (Seq Element)
    | EUser Text
    | EChannel Text
    deriving (Show, Eq)

data ElementStyle =
    Normal
    | Emph
    | Strong
    | Code
    | Edited
    | EditedRecently
    deriving (Eq, Show)
