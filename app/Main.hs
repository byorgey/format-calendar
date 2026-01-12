{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar
import Data.Yaml
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.Builder
import Text.Pandoc.Class qualified as Pandoc
import Text.Pandoc.Error qualified as Pandoc
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import Text.Pandoc.Options (WriterOptions (writerExtensions))
import Text.Pandoc.Writers.Markdown qualified as Pandoc
import Witch (from, into)

data ResourceType = PDF | Python | Kaggle | Haskell | LaTeX | Stream | YouTube | Agda | POGIL | Text | Disco | MP3
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
  FEmph :: Field -> Field
  FBold :: Field -> Field
  FCode :: Text -> Field
  FSeq :: [Field] -> Field
  FResource :: ResourceType -> Text -> Field
  FLink :: Text -> Maybe Text -> Field
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
      [("link", Object v')] -> FLink <$> v' .: "text" <*> v' .:? "url"
      [("emph", v')] -> FEmph <$> parseJSON v'
      [("bold", v')] -> FBold <$> parseJSON v'
      [("code", v')] -> FCode <$> parseJSON v'
      [(resName, v')] -> case M.lookup resName resourcesByName of
        Just r -> FResource r <$> parseJSON v'
        Nothing -> fail $ "Unknown resource type '" ++ T.unpack resName ++ "'"
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
        .: "Date"
      <*> parseJSON (Object v)

data Period = A Int | B Int deriving (Eq, Show)

periodDays :: Period -> [DayOfWeek]
periodDays = \case
  A {} -> [Monday, Wednesday, Friday]
  B {} -> [Tuesday, Thursday]

instance FromJSON Period where
  parseJSON = withText "Period" $ \v ->
    case into @String v of
      ['A', n] -> return $ A (read [n])
      ['B', n] -> return $ B (read [n])
      _ -> fail "Unrecognized period"

data CourseCalendar = CourseCalendar
  { name :: Text
  , semester :: Text
  , period :: Period
  , columns :: [Text]
  , headers :: Map Text Text
  , entries :: [Entry]
  , codeDir :: Text
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
        .: "period"
      <*> v
        .: "columns"
      <*> v
        .: "headers"
      <*> v
        .: "entries"
      <*> v
        .:? "codedir"
        .!= "lectures"

-- Need to generalize to generate multiple rows from a single Entry
--   - Blank rows for weekends
--   - Single entry with a list in some fields should generate multiple rows?
toPandoc :: CourseCalendar -> Pandoc
toPandoc cal =
  doc $
    simpleTable hdrs rows
 where
  hdrs = map (plain . fromString . from @Text . lookupHeader) (columns cal)
  lookupHeader c = fromMaybe c (M.lookup c (headers cal))
  es = entries cal

  withSpacers :: [Maybe Entry]
  withSpacers = Just (head es) : concat (zipWith insertSpace es (tail es))
  insertSpace e1 e2
    | overWeekend (entryDate e1) (entryDate e2) = [Nothing, Just e2]
    | otherwise = [Just e2]
  rows :: [[Blocks]]
  rows = concatMap (maybe [[]] (renderEntry cal)) withSpacers

renderEntry :: CourseCalendar -> Entry -> [[Blocks]]
renderEntry cal e = transposeL (map (renderRows cal e) (columns cal))

transposeL :: Monoid a => [[a]] -> [[a]]
transposeL [] = []
transposeL cols = transpose cols'
 where
  maxLen = maximum (map length cols)
  cols' = map pad cols

  pad col = col ++ replicate (maxLen - length col) mempty

renderRows :: CourseCalendar -> Entry -> Text -> [Blocks]
renderRows cal e hdr = maybe [] (map (plain . renderField cal) . unRows) (M.lookup hdr (fields e))

renderField :: CourseCalendar -> Field -> Inlines
renderField cal = \case
  FDate d -> renderDate (period cal) d
  FPlain t -> text t
  FEmph f -> emph (renderField cal f)
  FBold f -> strong (renderField cal f)
  FCode f -> link (codeDir cal <> "/" <> f) "" (text f)
  FSeq fs -> foldl1 (\x y -> x <> " " <> y) (map (renderField cal) fs)
  FResource rsc url -> link url "" (image (resourceIcon rsc) "" (tshowlow rsc))
  FLink txt Nothing -> text txt
  FLink txt (Just url) -> link url "" (text txt)

resourceIcon :: ResourceType -> Text
resourceIcon rsc = "icons/" <> from @String (map toLower (show rsc)) <> ".png"

renderDate :: Period -> Day -> Inlines
renderDate pd d = dateStyle $ dowAbbr <> " " <> tshow dom <> " " <> monthAbbr
 where
  dow = dayOfWeek d
  dowAbbr = case dow of
    Thursday -> "Th"
    Sunday -> "Su"
    _ -> text (from @String [head (show dow)])
  (_, month, dom) = toGregorian d

  dateStyle
    | dow `elem` periodDays pd = strong
    | otherwise = emph

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
  calFile <- case args of
    [f] -> return f
    _ -> do
      hPutStrLn stderr "No calendar file specified, defaulting to 'calendar.yaml'"
      return "calendar.yaml"
  res <- decodeFileEither calFile :: IO (Either ParseException CourseCalendar)
  case res of
    Left exc -> putStr $ prettyPrintParseException exc
    Right cal -> do
      pandocRes <- Pandoc.runIO $ do
        cm <- Pandoc.writeMarkdown def {writerExtensions = githubMarkdownExtensions} (toPandoc cal)
        liftIO . putStr $ from @Text cm
      Pandoc.handleError pandocRes
