module Cheapskate_QCA where

import Cheapskate.Types
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck

instance Arbitrary Block where
    arbitrary = oneof [ Para <$> arbitrary
                      , Header <$> arbitrary <*> arbitrary
                      , Blockquote <$> arbitrary
                      , List <$> arbitrary <*> arbitrary <*> arbitrary
                      , CodeBlock <$> arbitrary <*> arbitrary
                      , HtmlBlock <$> arbitrary
                      , pure HRule
                      ]

instance Arbitrary Inline where
    arbitrary = oneof [ Str <$> arbitrary
                      , pure Space
                      , pure SoftBreak
                      , pure LineBreak
                      , Emph <$> arbitrary
                      , Strong <$> arbitrary
                      , Code <$> arbitrary
                      , Link <$> arbitrary <*> arbitrary <*> arbitrary
                      , Image <$> arbitrary <*> arbitrary <*> arbitrary
                      , Entity <$> arbitrary
                      , RawHtml <$> arbitrary
                      ]

instance Arbitrary ListType where
    arbitrary = oneof [ Bullet <$> arbitrary
                      , Numbered <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary NumWrapper where
    arbitrary = elements [ PeriodFollowing, ParenFollowing ]

instance Arbitrary CodeAttr where
    arbitrary = CodeAttr <$> arbitrary <*> arbitrary
