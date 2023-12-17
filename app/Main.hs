{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (toLower)
import Data.Default (def)
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Yaml
import System.Environment (getArgs)
import Text.Pandoc.Builder
import Text.Pandoc.Class qualified as Pandoc
import Text.Pandoc.Error qualified as Pandoc
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import Text.Pandoc.Options (WriterOptions (writerExtensions))
import Text.Pandoc.Writers.Markdown qualified as Pandoc
import Witch (from)

data ResourceType = PDF | Python | Kaggle | Haskell | LaTeX | Stream | YouTube
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

resourcesByName :: Map Text ResourceType
resourcesByName =
  M.fromList $
    map ((from @String . map toLower . show) &&& id) [minBound .. maxBound]

newtype Rows = Rows {unRows :: [Field]}
  deriving (Eq, Show)

instance FromJSON Rows where
  parseJSON v = case v of
    Object m -> case M.assocs (KM.toMapText m) of
      [("rows", x)] -> Rows <$> parseJSON x
      _ -> Rows . (: []) <$> parseJSON v
    _ -> Rows . (: []) <$> parseJSON v

data Field where
  FDate :: Day -> Field
  FPlain :: Text -> Field
  FSeq :: [Field] -> Field
  FResource :: ResourceType -> Text -> Field
  FLink :: Text -> Text -> Field
  deriving (Eq, Show)

-- 2023-10-19   -- Date (try parsing as date first)

-- foo          -- Plain (if parsing as a date fails)

-- - foo        -- ISeq
-- - bar

-- rows:        -- NSeq
--   - foo
--   - bar

-- pdf: foo.pdf  -- Resource

-- link:         -- Link
--   text: foo
--   url: blah

instance FromJSON Field where
  parseJSON v = case v of
    String t -> (FDate <$> parseJSON v) <|> pure (FPlain t)
    Array _ -> FSeq <$> parseJSON v
    Object m -> case M.assocs (KM.toMapText m) of
      [("link", Object v')] -> FLink <$> v' .: "text" <*> v' .: "url"
      [(resName, v')] -> case M.lookup resName resourcesByName of
        Just r -> FResource r <$> parseJSON v'
        Nothing -> fail "Unknown resource type"
      _ -> fail "Unknown fields"
    _ -> fail "Can't parse field"

data Entry = Entry
  { entryDate :: Day
  , fields :: Map Text Rows
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
    simpleTable headers rows
 where
  headers = map (plain . fromString . from @Text) (columns cal)
  es = entries cal

  withSpacers :: [Maybe Entry]
  withSpacers = Just (head es) : concat (zipWith insertSpace es (tail es))
  insertSpace e1 e2
    | overWeekend (entryDate e1) (entryDate e2) = [Nothing, Just e2]
    | otherwise = [Just e2]
  rows :: [[Blocks]]
  rows = concatMap (maybe [[]] (renderEntry (columns cal))) withSpacers

renderEntry :: [Text] -> Entry -> [[Blocks]]
renderEntry cols e = transposeL (map (renderRows e) cols)

transposeL :: Monoid a => [[a]] -> [[a]]
transposeL [] = []
transposeL cols = transpose cols'
 where
  maxLen = maximum (map length cols)
  cols' = map pad cols

  pad col = col ++ replicate (maxLen - length col) mempty

renderRows :: Entry -> Text -> [Blocks]
renderRows e hdr = maybe [] (map (plain . renderField) . unRows) (M.lookup hdr (fields e))

renderField :: Field -> Inlines
renderField (FDate d) = renderDate d
renderField (FPlain t) = text t
renderField (FSeq fs) = foldl1 (\x y -> x <> " " <> y) (map renderField fs)
renderField (FResource rsc url) = link url "" (image (resourceIcon rsc) "" (tshowlow rsc))
renderField (FLink txt url) = link url "" (text txt)

resourceIcon :: ResourceType -> Text
resourceIcon rsc = "icons/" <> from @String (map toLower (show rsc)) <> ".png"

renderDate :: Day -> Inlines
renderDate d = dowChar <> " " <> tshow dom <> " " <> monthAbbr
 where
  dowChar = text (from @String [head (show (dayOfWeek d))])
  (_, month, dom) = toGregorian d

  monthAbbr = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] !! month

tshow :: Show a => a -> Inlines
tshow = text . from @String . show

tshowlow :: Show a => a -> Inlines
tshowlow = text . from @String . map toLower . show

-- Determine whether a weekend falls in between two dates
overWeekend :: Day -> Day -> Bool
overWeekend d1 d2 = dayOfWeek d1 >= dayOfWeek d2

main :: IO ()
main = do
  args <- getArgs
  case args of
    [calFile] -> do
      res <- decodeFileEither calFile :: IO (Either ParseException CourseCalendar)
      case res of
        Left exc -> putStr $ prettyPrintParseException exc
        Right cal -> do
          pandocRes <- Pandoc.runIO $ do
            cm <- Pandoc.writeMarkdown def {writerExtensions = githubMarkdownExtensions} (toPandoc cal)
            liftIO . putStr $ from @Text cm
          Pandoc.handleError pandocRes
    _ -> putStrLn "Usage: format-calendar <calendar.yaml>"
