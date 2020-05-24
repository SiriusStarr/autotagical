{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Parser
-- Description : Input/output tag formats and tag parsing
--
-- Contains all necessary types for input and output formats, validation for
-- them, and Dhall instances, as well as the functions for parsing file
-- information (tags, extensions, etc.) from raw file names.
module Parser
  ( -- * Input/Output Settings

    -- ** Common Types
    Order (..),
    TagValueOrder (..),

    -- ** Input
    Separator (..),
    InputFormat (..),

    -- ** Output
    OutputTaggedValueFormat (..),
    OutputFormat (..),

    -- * File Information
    FileInfo (..),
    FileParser (..),
    fileParser,
    parseFileInfo,

    -- * Internal For Testing
    Parser,
    SeparatorRequirement (..),
    TaggedValueFormat (..),
    separatorParser,
    taggedValueParser,
    tagParser,
    validateInputFormat,
    validateOutputFormat,
  )
where

import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Either.Combinators (rightToMaybe)
import Data.Either.Validation (Validation (..))
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Dhall (Decoder (..), FromDhall (..), autoWith, extractError)
import qualified Dhall as D
import GHC.Generics (Generic)
import SafeType (SafeText, safeText, validateText)
import System.FilePath (splitExtension, takeFileName)
import Tag (Tag (..), TagValue (..), Tags (..))
import Text.Megaparsec
  ( (<|>),
    Parsec,
    anySingle,
    eof,
    failure,
    lookAhead,
    many,
    manyTill,
    match,
    notFollowedBy,
    parseMaybe,
    someTill,
    takeWhile1P,
    try,
  )
import Text.Megaparsec.Char (space1, string)

-- * Input/Output

-- ** Common Types

-- | The order of file name vs tags on a file
data Order
  = NameTagsExtension
  | TagsNameExtension
  deriving (Eq, FromDhall, Generic)

instance Show Order where
  show NameTagsExtension = "File Name, Tags, Extension"
  show TagsNameExtension = "Tags, File Name, Extension"

-- | The order of tags vs values in a tagged value
data TagValueOrder
  = TagBeforeValue
  | ValueBeforeTag
  deriving (Eq, FromDhall, Generic)

instance Show TagValueOrder where
  show TagBeforeValue = "Tags Before Values"
  show ValueBeforeTag = "Values Before Tags"

-- ** Input

-- | Whether a separator is forbidden, optional, or required at the start or
--   end of a list of tags.
data SeparatorRequirement
  = Forbidden
  | Optional
  | Required
  deriving (Eq, FromDhall, Generic, Show)

-- | A separator in a tagging format; may be any amount of whitespace or a
--   keyword.
data Separator
  = Whitespace
  | Keyword SafeText
  deriving (Eq, FromDhall, Generic, Show)

-- | The format of tagged values in input (if any)
data TaggedValueFormat
  = TaggedValueFormat
      { order :: TagValueOrder,
        preTagValueSeparator :: Maybe Separator,
        tagValueSeparator :: Separator,
        postTagValueSeparator :: Maybe Separator,
        pureTagsAllowed :: Bool
      }
  deriving (Eq, FromDhall, Generic)

instance Show TaggedValueFormat where
  show
    TaggedValueFormat
      { order,
        preTagValueSeparator,
        tagValueSeparator,
        postTagValueSeparator,
        pureTagsAllowed
      } =
      concat
        [ "Tagged Value Format:\n  Order: ",
          show order,
          "\n  Opening Separator: ",
          maybe "None" show preTagValueSeparator,
          "\n  Tag-Value Separator: ",
          show tagValueSeparator,
          "\n  Closing Separator: ",
          maybe "None" show postTagValueSeparator,
          "\n  Pure Tags Allowed: ",
          show pureTagsAllowed
        ]

-- | The expected input format of files.
data InputFormat
  = InputFormat
      { tagOrder :: Order,
        openingSeparator :: Separator,
        leadingTagSeparator :: SeparatorRequirement,
        tagSeparator :: Separator,
        taggedValueFormat :: Maybe TaggedValueFormat,
        trailingTagSeparator :: SeparatorRequirement,
        closingSeparator :: Separator
      }
  deriving (Eq, Generic)

instance Show InputFormat where
  show
    InputFormat
      { tagOrder,
        openingSeparator,
        leadingTagSeparator,
        tagSeparator,
        taggedValueFormat,
        trailingTagSeparator,
        closingSeparator
      } =
      concat
        [ "Input Format:\nOrder: ",
          show tagOrder,
          "\nOpening Separator: ",
          show openingSeparator,
          "\nLeading Tag Separator: ",
          show leadingTagSeparator,
          "\nTag Separator: ",
          show tagSeparator,
          "\n",
          maybe "Tagged Values: Not Allowed" show taggedValueFormat,
          "\nTrailing Tag Separator: ",
          show trailingTagSeparator,
          "\nClosing Separator: ",
          show closingSeparator
        ]

-- | Check if two separators can be confused by the parser.
confusableSeparator :: ((Text, Separator), (Text, Separator)) -> Validation Text ()
confusableSeparator ((p1, Whitespace), (p2, Whitespace)) =
  Failure $ T.concat ["The ", p1, " and ", p2, " were both Whitespace!"]
confusableSeparator ((p1, Whitespace), (p2, Keyword t))
  | T.any isSpace $ safeText t =
    Failure $
      T.concat
        ["The ", p1, " separator was Whitespace, but the ", p2, " separator keyword ", T.pack (show t), " contains whitespace!"]
  | otherwise = Success ()
confusableSeparator ((p1, Keyword t1), (p2, Keyword t2))
  | safeText t1 `T.isInfixOf` safeText t2 =
    Failure $
      T.concat
        ["The ", p1, " separator keyword ", T.pack (show t1), " is an infix of the ", p2, " separator keyword ", T.pack (show t2), "!"]
  | safeText t2 `T.isInfixOf` safeText t1 =
    Failure $
      T.concat
        ["The ", p2, " separator keyword ", T.pack (show t2), " is an infix of the ", p1, " separator keyword ", T.pack (show t1), "!"]
  | otherwise = Success ()
-- Symmetrical whitespace + keyword case
confusableSeparator (s1, s2) = confusableSeparator (s2, s1)

-- | Confirm that an `InputFormat` does not have separators that will confuse
--   the parser.
validateInputFormat :: InputFormat -> Validation Text ()
validateInputFormat (InputFormat _ open _ tag tvFormat _ close) =
  traverse_
    confusableSeparator
    $ [(("opening", open), ("tag", tag)), (("tag", tag), ("closing", close))]
      ++ tvChecks
  where
    tvChecks =
      case tvFormat of
        Just (TaggedValueFormat _ pre tv post _) ->
          (("tagged value", tv), ("tag", tag)) : preCheck ++ postCheck
          where
            preCheck =
              maybe
                []
                ( \t ->
                    [ (("pre-tagged value", t), ("opening", open)),
                      (("pre-tagged value", t), ("tag", tag))
                    ]
                )
                pre
            postCheck =
              maybe
                []
                ( \t ->
                    [ (("post-tagged value", t), ("tag", tag)),
                      (("post-tagged value", t), ("closing", close))
                    ]
                )
                post
        Nothing -> []

-- | `FromDhall` instance that checks for possibly confusable separators.
instance FromDhall InputFormat where
  autoWith opts =
    validate $
      D.record
        ( InputFormat
            <$> D.field "tagOrder" (autoWith opts)
              <*> D.field "openingSeparator" (autoWith opts)
              <*> D.field "leadingTagSeparator" (autoWith opts)
              <*> D.field "tagSeparator" (autoWith opts)
              <*> D.field "taggedValueFormat" (autoWith opts)
              <*> D.field "trailingTagSeparator" (autoWith opts)
              <*> D.field "closingSeparator" (autoWith opts)
        )
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success p -> case validateInputFormat p of
              Failure err -> extractError err
              Success () -> Success p
            Failure f -> Failure f

-- ** Output

-- | The format of tagged values in output
data OutputTaggedValueFormat
  = OutputTaggedValueFormat
      { order :: TagValueOrder,
        preTagValueSeparator :: Maybe SafeText,
        tagValueSeparator :: SafeText,
        postTagValueSeparator :: Maybe SafeText,
        pureTagsAllowed :: Bool
      }
  deriving (Eq, FromDhall, Generic)

instance Show OutputTaggedValueFormat where
  show
    OutputTaggedValueFormat
      { order,
        preTagValueSeparator,
        tagValueSeparator,
        postTagValueSeparator,
        pureTagsAllowed
      } =
      concat
        [ "Tagged Value Format:\n  Order: ",
          show order,
          "\n  Opening Separator: ",
          maybe "None" show preTagValueSeparator,
          "\n  Tag-Value Separator: ",
          show tagValueSeparator,
          "\n  Closing Separator: ",
          maybe "None" show postTagValueSeparator,
          "\n  Pure Tags Allowed: ",
          show pureTagsAllowed
        ]

-- | Defines the format to write output tags in
data OutputFormat
  = OutputFormat
      { tagOrder :: Order,
        openingSeparator :: SafeText,
        leadingTagSeparator :: Bool,
        tagSeparator :: SafeText,
        taggedValueFormat :: Maybe OutputTaggedValueFormat,
        trailingTagSeparator :: Bool,
        closingSeparator :: SafeText
      }
  deriving (Eq, Generic)

instance Show OutputFormat where
  show
    OutputFormat
      { tagOrder,
        openingSeparator,
        leadingTagSeparator,
        tagSeparator,
        taggedValueFormat,
        trailingTagSeparator,
        closingSeparator
      } =
      concat
        [ "Output Format:\nOrder: ",
          show tagOrder,
          "\nOpening Separator: ",
          show openingSeparator,
          "\nLeading Tag Separator: ",
          show leadingTagSeparator,
          "\nTag Separator: ",
          show tagSeparator,
          "\n",
          maybe "Tagged Values: Not Allowed" show taggedValueFormat,
          "\nTrailing Tag Separator: ",
          show trailingTagSeparator,
          "\nClosing Separator: ",
          show closingSeparator
        ]

-- | Check if two output separators can be confused by the parser
confusableOutputSeparator :: ((Text, SafeText), (Text, SafeText)) -> Validation Text ()
confusableOutputSeparator ((p1, safeText -> t1), (p2, safeText -> t2))
  | T.all isSpace t1 && T.any isSpace t2 =
    Failure $
      T.concat
        ["The ", p1, " separator ", T.pack (show t1), " is (probably) meant to be whitespace, but the ", p2, " separator ", T.pack (show t2), "contains whitespace!"]
  | T.all isSpace t2 && T.any isSpace t1 =
    Failure $
      T.concat
        ["The ", p2, " separator ", T.pack (show t2), " is (probably) meant to be whitespace, but the ", p1, " separator ", T.pack (show t1), "contains whitespace!"]
  | t1 `T.isInfixOf` t2 =
    Failure $
      T.concat
        ["The ", p1, " separator ", T.pack (show t1), " is an infix of the ", p2, " separator ", T.pack (show t2), "!"]
  | t2 `T.isInfixOf` t1 =
    Failure $
      T.concat
        ["The ", p2, " separator ", T.pack (show t2), " is an infix of the ", p1, " separator ", T.pack (show t1), "!"]
  | otherwise = Success ()

-- | Confirm that an `OutputFormat` does not have separators that will confuse
--   the parser.
validateOutputFormat :: OutputFormat -> Validation Text ()
validateOutputFormat (OutputFormat _ open _ tag tvFormat _ close) =
  traverse_
    confusableOutputSeparator
    $ [(("opening", open), ("tag", tag)), (("tag", tag), ("closing", close))]
      ++ tvChecks
  where
    tvChecks =
      case tvFormat of
        Just (OutputTaggedValueFormat _ pre tv post _) ->
          (("tagged value", tv), ("tag", tag)) : preCheck ++ postCheck
          where
            preCheck =
              maybe
                []
                ( \t ->
                    [ (("pre-tagged value", t), ("opening", open)),
                      (("pre-tagged value", t), ("tag", tag))
                    ]
                )
                pre
            postCheck =
              maybe
                []
                ( \t ->
                    [ (("post-tagged value", t), ("tag", tag)),
                      (("post-tagged value", t), ("closing", close))
                    ]
                )
                post
        Nothing -> []

instance FromDhall OutputFormat where
  autoWith opts =
    validate $
      D.record
        ( OutputFormat <$> D.field "tagOrder" (autoWith opts)
            <*> D.field "openingSeparator" (autoWith opts)
            <*> D.field "leadingTagSeparator" (autoWith opts)
            <*> D.field "tagSeparator" (autoWith opts)
            <*> D.field "taggedValueFormat" (autoWith opts)
            <*> D.field "trailingTagSeparator" (autoWith opts)
            <*> D.field "closingSeparator" (autoWith opts)
        )
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success p -> case validateOutputFormat p of
              Failure err -> extractError err
              Success () -> Success p
            Failure f -> Failure f

-- * File Information

-- | All information about a file after parsing it.
data FileInfo
  = FileInfo
      { fileTags :: Tags,
        rawTags :: SafeText,
        originalName :: SafeText,
        extension :: Maybe SafeText
      }
  deriving (Eq, Generic)

instance Show FileInfo where
  show FileInfo {fileTags, rawTags, originalName, extension} =
    concat
      [ "File Info:\n  ",
        show fileTags,
        "\n  Raw Tags: ",
        show rawTags,
        "\n  Original Name: ",
        show originalName,
        "\n  Extension: ",
        maybe "None" show extension
      ]

-- | Parser type for Megaparsec; no custom error elements.
type Parser = Parsec Void Text

-- | Non-greedy version of `Text.Megaparsec.sepBy1`
sepBy1' :: Parser a -> Parser () -> Parser [a]
sepBy1' p sep = (:) <$> p <*> many (try $ sep *> p)

-- | Non-greedy version of `Text.Megaparsec.sepEndBy1`
sepEndBy1' :: Parser a -> Parser () -> Parser [a]
sepEndBy1' p sep = (:) <$> p <*> many (try $ sep *> p) <* (sep <|> pure ())

-- | Non-greedy version of `Text.Megaparsec.endBy1`
endBy1' :: Parser a -> Parser () -> Parser [a]
endBy1' p sep = (:) <$> (p <* sep) <*> many (try $ p <* sep)

-- | Convert a `Separator` from Dhall into a parser.
separatorParser :: Separator -> Parser ()
separatorParser = \case
  Whitespace -> space1
  Keyword t -> () <$ string (safeText t)

-- | Given a `TaggedValueFormat`, a tag separator parser, and a closing
--   separator, create a parser that returns the tag name and the tag value (or
--  `Present` if applicable).
taggedValueParser :: Maybe TaggedValueFormat -> Parser () -> Parser () -> Parser (Tag, TagValue)
taggedValueParser f tagSep closeSep =
  parser f >>= \case
    Left _ -> failure Nothing S.empty
    Right t -> pure t
  where
    parser :: Maybe TaggedValueFormat -> Parser (Either Text (Tag, TagValue))
    parser Nothing = plainTagParser
    parser (Just (TaggedValueFormat ord pre (separatorParser -> tv) post pureAllowed))
      | pureAllowed = try taggedValue <|> plainTagParser
      | otherwise = taggedValue
      where
        taggedValue = pack <$> parseFirst <*> parseLast
        parseFirst =
          T.pack
            <$> ( maybe (pure ()) separatorParser pre
                    *> someTill anySingle (lookAhead tv <|> end)
                )
        parseLast =
          T.pack
            <$ tv <*> someTill anySingle (maybe end separatorParser post)
        pack (validateText -> Right t1) (validateText -> Right t2) = case ord of
          TagBeforeValue -> Right (Tag t1, Value t2)
          ValueBeforeTag -> Right (Tag t2, Value t1)
        pack _ _ = Left ""
    -- Parse characters until encountering either a tag separator or the
    -- closing separator.
    plainTagParser =
      (\t -> (,Present) . Tag <$> validateText (T.pack t))
        <$> manyTill anySingle end
    end = lookAhead $ tagSep <|> closeSep

-- | Given the components of an input format, parse tags from it, returning them
--   along with the raw text of the tags.
tagParser ::
  Separator ->
  SeparatorRequirement ->
  Separator ->
  Maybe TaggedValueFormat ->
  SeparatorRequirement ->
  Separator ->
  Parser (Text, Tags)
tagParser
  (separatorParser -> open)
  leadReq
  (separatorParser -> tagSep)
  tv
  trailReq
  (separatorParser -> close) =
    second (Tags . M.fromList) <$> match (open *> leading *> ts <* close)
    where
      leading = case leadReq of
        Forbidden ->
          notFollowedBy tagSep
        Optional ->
          tagSep <|> pure ()
        Required ->
          tagSep
      t = taggedValueParser tv tagSep close
      ts = case trailReq of
        Forbidden ->
          sepBy1' t tagSep
        Optional ->
          sepEndBy1' t tagSep
        Required ->
          endBy1' t tagSep

-- | A parser to parse names and tags from file names.  Info is returned as
--   (Name, (Raw Tags, Tags))
newtype FileParser = FileParser {parser :: Parser (Text, (Text, Tags))}

-- | Given an input format, attempt to parse all file info from an
--   extension-less file name.
fileParser :: InputFormat -> FileParser
fileParser (InputFormat ord open leading tagSep tv trailing close) =
  FileParser $ case ord of
    NameTagsExtension {} ->
      (,) <$> (T.pack <$> someTill anySingle (lookAhead (ts *> eof))) <*> ts
    TagsNameExtension {} ->
      flip (,) <$> ts <*> takeWhile1P Nothing (const True)
  where
    ts = tagParser open leading tagSep tv trailing close

-- | Given an input format and the path to a file, try to parse its info.
parseFileInfo :: Order -> FileParser -> FilePath -> Maybe FileInfo
parseFileInfo ord (parser -> p) (takeFileName -> file) =
  go file ""
    >>= case ord of
      NameTagsExtension ->
        makeFileInfo
      TagsNameExtension ->
        \info@(ts, raw, n, e) ->
          case splitExtensions $ T.unpack n of
            ("", e' : es) ->
              -- There has to be some file name, so the last extension is that
              makeFileInfo (ts, raw, T.pack e', T.concat (T.pack <$> es) <> e)
            (_, []) ->
              makeFileInfo info
            (n', es) ->
              -- Concatenate any more extensions we greedily parsed to others
              makeFileInfo (ts, raw, T.pack n', T.concat (T.pack <$> es) <> e)
  where
    go :: FilePath -> Text -> Maybe (Tags, Text, Text, Text)
    go f ext
      -- With a successful parse, finished.
      | Just (n, (raw, ts)) <- parseMaybe p (T.pack f) =
        Just (ts, raw, n, ext)
      -- If parse failed and no extensions remain, file can't be parsed
      | (_, "") <- splitExtension f = Nothing
      -- Otherwise, split an extension off and try again.
      | (f', ext') <- splitExtension f = go f' (T.pack ext' <> ext)
    makeFileInfo (ts, raw, name, ext) =
      rightToMaybe $
        FileInfo ts
          <$> validateText raw
            <*> validateText name
            <*> ( case ext of
                    "" -> Right Nothing
                    e -> Just <$> validateText e
                )

-- | Return all possible extensions as a list of extensions.
--   Examples are given below:
--
-- * `"file"` -> `("file", [])`
-- * `"file.ext"` -> `("file", [".ext"])`
-- * `"file.tar.gz"` -> `("file", [".tar", ".gz"])`
-- * `".dotfile"` -> `("", [".dotfile"])`
splitExtensions :: FilePath -> (FilePath, [String])
splitExtensions path = go (path, [])
  where
    go (f, es)
      | (_, "") <- splitExtension f = (f, es)
      | (f', e) <- splitExtension f = go (f', e : es)
