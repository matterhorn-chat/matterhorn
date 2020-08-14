{- |

This module provides an INI schema validator that is able to track unused
sections and fields in order to report warning messages to the user.


 -}
module Matterhorn.Config.Schema
  ( IniParser
  , parseIniFile
  , (<!>)

  , Fatal(..)
  , fatalString

  , Warning(..)
  , warningString

  , section
  , sectionMb
  , fieldMbOf
  , fieldMb
  , field
  , fieldDefOf
  , fieldFlagDef

  -- * Re-exports
  , number
  , string
  , listWithSeparator
  ) where

import           Prelude ()
import           Matterhorn.Prelude

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Control.Monad
import           Data.Ini.Config (flag, number, listWithSeparator, string)
import           Data.Ini.Config.Raw


------------------------------------------------------------------------

newtype Parser e t a = Parser { unParser :: e -> Map NormalizedText (NonEmpty t) -> Either Fatal (Map NormalizedText (NonEmpty t), [Warning], a) }

instance Functor (Parser e t) where
  fmap = liftM

instance Applicative (Parser e t) where
  pure x = Parser $ \_ s -> Right (s, [], x)
  (<*>) = ap

instance Monad (Parser e t) where
  m >>= f = Parser $ \e s0 ->
              do (s1, ws1, x1) <- unParser m e s0
                 (s2, ws2, x2) <- unParser (f x1) e s1
                 Right (s2, ws1++ws2, x2)



(<!>) :: Parser e t a -> Parser e t a -> Parser e t a
p <!> q = Parser $ \e s ->
  case unParser p e s of
    Right r -> Right r
    Left {} -> unParser q e s

getenv :: Parser e t e
getenv = Parser $ \e s -> Right (s, [], e)

request :: Text -> Parser e t (Maybe t)
request name = Parser $ \_ s ->
  let name' = normalize name in
  Right $!
  case Map.lookup name' s of
    Nothing                 -> (s , [], Nothing)
    Just (x NonEmpty.:| xs) -> (s', [], Just x)
      where
        s' = case NonEmpty.nonEmpty xs of
               Nothing -> Map.delete name' s
               Just ne -> Map.insert name' ne s

fatal :: Fatal -> Parser e t a
fatal e = Parser $ \_ _ -> Left e

warnings :: [Warning] -> IniParser ()
warnings ws = Parser $ \_ s -> Right (s, ws, ())

------------------------------------------------------------------------

type IniParser = Parser RawIni IniSection

type SectionParser = Parser IniSection IniValue

data Fatal
  = NoSection Text
  | MissingField IniSection Text
  | BadField IniSection IniValue String
  | ParseError String
  deriving Show

data Warning
  = UnusedSection IniSection
  | UnusedField IniSection IniValue
  deriving Show

------------------------------------------------------------------------

fatalString :: Fatal -> String
fatalString (NoSection name) = "No top-level section named " ++ show name
fatalString (MissingField sec name) = "Missing field " ++ show name ++ " in section " ++ show (isName sec)
fatalString (BadField sec val err) =
  "Line " ++ show (vLineNo val) ++
  " in section " ++ show (isName sec) ++
  ": " ++ err
fatalString (ParseError err) = err

warningString :: Warning -> String
warningString (UnusedSection sec) = "Unused section " ++ show (isName sec)
warningString (UnusedField sec val) =
  "Line " ++ show (vLineNo val) ++
  " in section " ++ show (isName sec) ++
  ": unused field"

------------------------------------------------------------------------

parseIniFile :: Text -> IniParser a -> Either Fatal ([Warning], a)
parseIniFile text parser =
  case parseRawIni text of
    Left e -> Left (ParseError e)
    Right ini ->
      let entries = Map.fromListWith (<>)
                       [ (k, pure v) | (k,v) <- toList (fromRawIni ini) ]
      in
      case unParser parser ini entries of
        Left e -> Left e
        Right (entries', ws, x) -> Right (ws ++ unused, x)
          where
            unused = [ UnusedSection e | e <- concatMap toList entries' ]

------------------------------------------------------------------------

section :: Text -> SectionParser a -> IniParser a
section name parser =
  do mb <- sectionMb name parser
     case mb of
       Nothing -> fatal (NoSection name)
       Just x  -> pure x

sectionMb :: Text -> SectionParser a -> IniParser (Maybe a)
sectionMb name parser =
  do mb <- request name
     case mb of
       Nothing -> pure Nothing
       Just sec ->
         let entries = Map.fromListWith (<>)
                       [ (k, pure v) | (k,v) <- toList (isVals sec) ]
         in
         case unParser parser sec entries of
           Left e -> fatal e
           Right (entries', ws, result) ->
             do warnings (ws ++ [ UnusedField sec v | v <- concatMap toList entries' ])
                pure (Just result)

------------------------------------------------------------------------

field :: Text -> SectionParser Text
field name =
  do mb <- fieldMb name
     case mb of
       Just x -> pure x
       Nothing ->
         do s <- getenv
            fatal (MissingField s name)

fieldMbOf :: Text -> (Text -> Either String a) -> SectionParser (Maybe a)
fieldMbOf name validate =
  do mb <- request name
     case mb of
       Nothing -> pure Nothing
       Just val ->
         case validate (getVal val) of
           Left e ->
             do sec <- getenv
                fatal (BadField sec val e)
           Right x -> pure (Just x)

fieldMb :: Text -> SectionParser (Maybe Text)
fieldMb name = fmap getVal <$> request name

fieldDefOf :: Text -> (Text -> Either String a) -> a -> SectionParser a
fieldDefOf name validate def = fromMaybe def <$> fieldMbOf name validate

fieldFlagDef :: Text -> Bool -> SectionParser Bool
fieldFlagDef name def = fieldDefOf name flag def

------------------------------------------------------------------------

getVal :: IniValue -> Text
getVal = T.strip . vValue
