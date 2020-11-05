module RichText_QCA
  ( genBlocks
  )
where

import qualified Data.Sequence as Seq
import           Network.Mattermost.Types ( PostId(..), Id(..) )
import           Network.Mattermost.QuickCheck (genText, genSeq)
import           Test.Tasty.QuickCheck
import           Matterhorn.Types.RichText

genBlocks :: Gen Blocks
genBlocks = Blocks <$> genSeq genBlock

seqOf :: Gen a -> Gen (Seq.Seq a)
seqOf g = fmap Seq.fromList (listOf g)

genBlock :: Gen Block
genBlock = oneof [ Para <$> genInlines
                 , Header <$> arbitrary <*> genInlines
                 , Blockquote <$> genBlocks
                 , List <$> genListType <*> genListSpacing <*> seqOf genBlocks
                 , CodeBlock <$> genCodeBlockInfo <*> genText
                 , HTMLBlock <$> genText
                 , return HRule
                 ]

genCodeBlockInfo :: Gen CodeBlockInfo
genCodeBlockInfo = CodeBlockInfo <$> genMaybe genText <*> genMaybe genText

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g =
    oneof [ Just <$> g
          , return Nothing
          ]

genInlines :: Gen Inlines
genInlines = Inlines <$> genSeq genInline

genInline :: Gen Inline
genInline = oneof [ EText <$> genText
                  , return ESpace
                  , return ESoftBreak
                  , return ELineBreak
                  , EEmph <$> genInlines
                  , EStrong <$> genInlines
                  , ECode <$> genInlines
                  , EStrikethrough <$> genInlines
                  , EHyperlink <$> (URL <$> genText) <*> genInlines
                  , EPermalink <$> (TeamURLName <$> genText) <*> genPostId <*> (genMaybe genInlines)
                  , EImage <$> (URL <$> genText) <*> genInlines
                  , ERawHtml <$> genText
                  ]

genPostId :: Gen PostId
genPostId = PI <$> Id <$> genText

genListType :: Gen ListType
genListType = oneof [ BulletList <$> arbitrary
                    , OrderedList <$> arbitrary <*> genEnumTy <*> genDelimTy
                    ]

genListSpacing :: Gen ListSpacing
genListSpacing = elements [ TightList, LooseList ]

genEnumTy :: Gen EnumeratorType
genEnumTy = elements [ Decimal, UpperAlpha, LowerAlpha, UpperRoman, LowerRoman ]

genDelimTy :: Gen DelimiterType
genDelimTy = elements [ Period, OneParen, TwoParens ]
