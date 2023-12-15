{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Yaml
import Text.Pandoc.Builder
import Text.Pandoc.Class qualified as Pandoc
import Text.Pandoc.Error qualified as Pandoc
import Text.Pandoc.Writers.CommonMark qualified as Pandoc
import Witch (from)

data ResourceType = PDF | Python | Haskell | LaTeX | Stream | YouTube
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Field where
  Date :: Day -> Field
  Plain :: Text -> Field
  ISeq :: [Field] -> Field
  NSeq :: [Field] -> Field
  Resource :: ResourceType -> Text -> Field
  Link :: Text -> Text -> Field
  deriving (Eq, Show)

instance FromJSON Field where
  parseJSON = undefined

data Entry = Entry
  { entryDate :: Day
  , fields :: Map Text Field
  }
  deriving (Eq, Show)

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v ->
    Entry
      <$> v
        .: "date"
      <*> parseJSON (Object v)

data CourseCalendar = CourseCalendar
  { name :: Text
  , semester :: Text
  , columns :: [Text]
  , entries :: [Entry]
  }
  deriving (Eq, Show)

instance FromJSON CourseCalendar where
  parseJSON = withObject "Course" $ \v ->
    CourseCalendar
      <$> v
        .: "name"
      <*> v
        .: "semester"
      <*> v
        .: "columns"
      <*> v
        .: "entries"

-- Need to generalize to generate multiple rows from a single Entry
--   - Blank rows for weekends
--   - Single entry with a list in some fields should generate multiple rows?
toPandoc :: CourseCalendar -> Pandoc
toPandoc cal =
  doc $
    simpleTable headers [[]]
 where
  headers = map (plain . fromString . from @Text) (columns cal)
  rows = map mkRow (entries cal)
  mkRow e = map (fromMaybe "" . (fields e !?)) (columns cal)

main :: IO ()
main = do
  res <- decodeFileEither "150-calendar.yaml" :: IO (Either ParseException CourseCalendar)
  case res of
    Left exc -> putStr $ prettyPrintParseException exc
    Right cal -> do
      print (toPandoc cal)
      res <- Pandoc.runIO $ do
        cm <- Pandoc.writeCommonMark def (toPandoc cal)
        liftIO $ print cm
      Pandoc.handleError res
