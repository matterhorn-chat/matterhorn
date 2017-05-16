module Cheapskate_QCA where

import           Cheapskate.Types
import qualified Data.Sequence as Seq ()
import           Network.Mattermost.QuickCheck (genText, genSeq)
import           Test.Tasty.QuickCheck

genBlocks :: Gen Blocks
genBlocks = genSeq genBlock

genBlock :: Gen Block
genBlock = oneof [ Para <$> genInlines
                 , Header <$> arbitrary <*> genInlines
                 , Blockquote <$> genBlocks
                 , List <$> arbitrary <*> genListType <*> listOf (genBlocks)
                 , CodeBlock <$> genCodeAttr <*> genText
                 , HtmlBlock <$> genText
                 , return HRule
                 ]

genInlines :: Gen Inlines
genInlines = genSeq genInline

genInline :: Gen Inline
genInline = oneof [ Str <$> genText
                  , return Space
                  , return SoftBreak
                  , return LineBreak
                  , Emph <$> genInlines
                  , Strong <$> genInlines
                  , Code <$> genText
                  , Link <$> genInlines <*> genText <*> genText
                  , Image <$> genInlines <*> genText <*> genText
                  , Entity <$> genText
                  , RawHtml <$> genText
                  ]

genListType :: Gen ListType
genListType = oneof [ Bullet <$> arbitrary
                      , Numbered <$> genNumWrapper <*> arbitrary
                      ]

genNumWrapper :: Gen NumWrapper
genNumWrapper = elements [ PeriodFollowing, ParenFollowing ]

genCodeAttr :: Gen CodeAttr
genCodeAttr = CodeAttr <$> genText <*> genText
