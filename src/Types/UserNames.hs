module Types.UserNames
  ( findUsernames
  , isNameFragment
  , takeWhileNameFragment
  )
where

import qualified Cheapskate as C
import           Data.Char ( isAlpha )
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T

import           Prelude ()
import           Prelude.MH


findUsernames :: C.Blocks -> S.Set T.Text
findUsernames = S.unions . F.toList . fmap blockFindUsernames

blockFindUsernames :: C.Block -> S.Set T.Text
blockFindUsernames (C.Para is) =
    inlineFindUsernames $ F.toList is
blockFindUsernames (C.Header _ is) =
    inlineFindUsernames $ F.toList is
blockFindUsernames (C.Blockquote bs) =
    findUsernames bs
blockFindUsernames (C.List _ _ bs) =
    S.unions $ F.toList $ findUsernames <$> bs
blockFindUsernames _ =
    mempty

inlineFindUsernames :: [C.Inline] -> S.Set T.Text
inlineFindUsernames [] = mempty
inlineFindUsernames (C.Str "@" : rest) =
    let (strs, remaining) = takeWhileNameFragment rest
    in if null strs
       then inlineFindUsernames remaining
       else S.insert (T.concat $ getInlineStr <$> strs) $ inlineFindUsernames remaining
inlineFindUsernames (_ : rest) =
    inlineFindUsernames rest

getInlineStr :: C.Inline -> T.Text
getInlineStr (C.Str s) = s
getInlineStr _ = ""

takeWhileNameFragment :: [C.Inline] -> ([C.Inline], [C.Inline])
takeWhileNameFragment [] = ([], [])
takeWhileNameFragment rest =
    let (strs, remaining) = break (not . isNameFragment) rest
        -- Does the last element in strs start with a letter? If
        -- not, move it to the remaining list. This avoids pulling
        -- punctuation-only tokens into usernames, e.g. "Hello,
        -- @foobar."
        (strs', remaining') =
            if length strs <= 1
            then (strs, remaining)
            else let (initStrs, [lastStr]) = splitAt (length strs - 1) strs
                 in if isAlpha $ T.head $ getInlineStr lastStr
                    then (strs, remaining)
                    else (initStrs, lastStr : remaining)
    in (strs', remaining')

isValidNameChar :: Char -> Bool
isValidNameChar c = isAlpha c || c == '_' || c == '.' || c == '-'

isNameFragment :: C.Inline -> Bool
isNameFragment (C.Str t) =
    not (T.null t) && isValidNameChar (T.head t)
isNameFragment _ = False
