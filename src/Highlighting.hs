{-# LANGUAGE PackageImports #-}

module Highlighting where

import qualified     Data.Array as A
import               Brick
import               Brick.Markup (markup, (@?))
import               Data.List (intercalate)
import qualified     Data.Text as T
import "text-markup" Data.Text.Markup
import               Lens.Micro.Platform ((^.))
import               Network.Mattermost
import               Network.Mattermost.Lenses
import               Text.Regex.Base.RegexLike (makeRegex, matchAll)
import               Text.Regex.TDFA.String

import               Themes

emailPattern :: Regex
emailPattern = makeRegex ("[[:alnum:]\\+]+@([[:alnum:]]+\\.)+([[:alnum:]]+)"::String)

urlPattern :: Regex
urlPattern = makeRegex ("https?://([[:alnum:]-]+\\.)*([[:alnum:]-]+)(:[[:digit:]]+)?(/[^[:space:]]*)"::String)

markdownPattern :: Regex
markdownPattern = makeRegex ("`[^`]+`"::String)

emojiPattern :: Regex
emojiPattern = makeRegex (":[^[:space:]:]+:"::String)

mkUsernamePattern :: [UserProfile] -> Regex
mkUsernamePattern users =
    makeRegex $ "(@|\\b)(" ++ intercalate "|" ((^.userProfileUsernameL) <$> users) ++ ")\\b"

findRegex :: T.Text -> Regex -> [(Int, Int)]
findRegex t r = concat $ A.elems <$> matchAll r (T.unpack t)

doMessageMarkup :: Regex -> T.Text -> Widget a
doMessageMarkup usernamePattern msg =
    let emailMatches    = findRegex msg emailPattern
        urlMatches      = findRegex msg urlPattern
        markdownMatches = findRegex msg markdownPattern
        usernameMatches = findRegex msg usernamePattern
        emojiMatches    = findRegex msg emojiPattern
        substr pos len s = T.take len $ T.drop pos s
        applyUsernameMatches mkup = foldr markUsername mkup usernameMatches
        markUsername (pos,len) m =
            let tag = attrForUsername (T.unpack $ substr pos len msg)
            in markRegion pos len tag m
        applyMatches matches tag mkup = foldr (\(pos,len) -> markRegion pos len tag) mkup matches

        pairs = fromMarkup $ applyMatches emailMatches    emailAttr    $
                             applyMatches markdownMatches markdownAttr $
                             applyMatches urlMatches      urlAttr      $
                             applyMatches emojiMatches    emojiAttr    $
                             applyUsernameMatches                      $
                             toMarkup msg ""
    in markup $ mconcat $ (uncurry (@?)) <$> pairs
