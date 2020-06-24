{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Renaming
-- Description : Renaming unnamed files and writing output tag formats
--
-- Contains all necessary types for file renaming and their Dhall instances, as
-- well as the functions to do the actual renaming, given file info.  Also
-- contains the functions to write files out as a specified output tag format.
module Renaming
  ( -- * Renaming
    nameBySchema,
    writeFileName,
    RenamingSchema (..),

    -- * Errors
    RenamingError (..),
    OutputFormatError (..),

    -- * Internal For Testing
    writeTag,
    sepToText,
    findRenamingRule,
    RenamingRule (..),
    trailingSepCollision,
    tagsSepCollision,
  )
where

import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Either.Validation (Validation (..), validationToEither)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (FromDhall)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import GHC.Natural (intToNatural)
import NameTemplate (FileName, NameTemplateError, translateFileName)
import Numeric.Natural (Natural)
import Parser
  ( FileInfo (..),
    InputFormat (..),
    Order (..),
    OutputFormat (..),
    OutputTaggedValueFormat (..),
    Separator (..),
    TagValueOrder (..),
  )
import Predicate (Predicate, checkPredicate)
import SafeType (NonEmptyList, SafeText, safeText)
import Tag (Tag (..), TagValue (..), Tags (..))

-- | Specify the way in which a file and tags failed to be written.
--
--   [`InvalidFileNameForOutputFormat`] The file name collides with an adjacent
--     separator specified in the `OutputFormat`, e.g. a file name contains
--     whitespace, but the opening separator is `Whitespace`.
--   [`InvalidTagForOutputFormat`] A tag on the file collides with a separator
--     specified in the `OutputFormat`, e.g. a tag contains whitespace, but the
--     tag separator is `Whitespace`.
--   [`InvalidTaggedValueForOutputFormat`] A tagged value on the file collides
--     with a separator specified in the `OutputFormat`, e.g. a value contains
--     whitespace, but the post-tagged value separator is `Whitespace`.
--   [`InvalidTagSeparatorCombination`] A more complex failure of the output
--     format was encountered.  This typically occurs when a combination of
--     multiple elements creates a separator, e.g. if a tagged value was written
--     as @tag=value@ but the tag separator was @Keyword "g=v"@.  This is
--     detected by the observed number of separators not equaling the expected
--     number, e.g. with 5 tagged values, 5 tagged value separators should be in
--     the output, so if 6 are observed, there is a problem.
--   [`NonTaggedValueWhenOutputForbidsThem`] A non-tagged value tag was
--     encountered with an output format that forbids them.
--   [`TaggedValueWhenOutputForbidsThem`] A tagged value was encountered with
--     an output format that forbids them.
data OutputFormatError
  = InvalidFileNameForOutputFormat {name :: Text, separator :: (Text, Text)}
  | InvalidTagForOutputFormat {tag :: Text, separator :: (Text, Text)}
  | InvalidTaggedValueForOutputFormat
      { tag :: Text,
        value :: Text,
        separator :: (Text, Text)
      }
  | InvalidTagSeparatorCombination
      { separator :: (Text, Text),
        expectedSeps :: Natural,
        tagText :: Text
      }
  | NonTaggedValueWhenOutputForbidsThem {tag :: Text}
  | TaggedValueWhenOutputForbidsThem {tag :: Text, value :: Text}
  deriving (Eq)

instance Show OutputFormatError where
  show InvalidFileNameForOutputFormat {name, separator} =
    concat
      [ "The file name ",
        show name,
        " conflicted with the adjacent ",
        T.unpack $ fst separator,
        " separator ",
        show $ snd separator,
        " specified in the output format."
      ]
  show InvalidTagForOutputFormat {tag, separator} =
    concat
      [ "The tag ",
        show tag,
        " conflicted with the ",
        T.unpack $ fst separator,
        " separator ",
        show $ snd separator,
        " specified in the output format."
      ]
  show InvalidTaggedValueForOutputFormat {tag, value, separator} =
    concat
      [ "The tagged value ",
        show tag,
        " = ",
        show value,
        " conflicted with the ",
        T.unpack $ fst separator,
        " separator ",
        show $ snd separator,
        " specified in the output format."
      ]
  show InvalidTagSeparatorCombination {separator, expectedSeps, tagText} =
    concat
      [ "A complex failure of the output format was encountered.  This typically occurs when a combination of multiple elements creates a separator, e.g. if a tagged value was written as \"tag=value\" but the tag separator was the keyword \"g=v\".  The ",
        T.unpack $ fst separator,
        " separator ",
        show $ snd separator,
        " was only expected to be encountered ",
        show expectedSeps,
        " times but was encountered more in the output tags: ",
        show tagText
      ]
  show NonTaggedValueWhenOutputForbidsThem {tag} =
    "The output format does not allow pure tags, only tagged values, but the pure tag " <> show tag <> " was encountered."
  show TaggedValueWhenOutputForbidsThem {tag, value} =
    concat
      [ "The output format does not allow tagged values, but the tagged value ",
        show tag,
        " = ",
        show value,
        " was encountered."
      ]

-- | Specify the way in which an unnamed file failed to be renamed
--
--   [`NameTemplateErrors`] A `RenamingRule` was found for the file, but the
--   file name template failed with one or more errors.
--   [`NoRenamingMatch`] No valid `RenamingRule` was found, i.e. no predicate
--   evaluated to true for the file.
data RenamingError
  = NameTemplateErrors {file :: FileInfo, errors :: [NameTemplateError]}
  | NoRenamingMatch {file :: FileInfo}
  deriving (Eq)

instance Show RenamingError where
  show NameTemplateErrors {file, errors} =
    unlines $
      [ "A renaming rule was found for the file:",
        show file,
        "but the following errors occurred while translating the name template:"
      ]
        ++ fmap show errors
  show NoRenamingMatch {file} = "No renaming match was found (i.e. no renaming rule predicate evaluated to True) for the following file:\n" <> show file

-- | A rule with a filtering `Predicate` and a `FileName` to specify file
--   renaming.
data RenamingRule
  = RenamingRule
      { name :: FileName,
        predicate :: Predicate
      }
  deriving (Eq, FromDhall, Generic)

instance Show RenamingRule where
  show RenamingRule {name, predicate} =
    unlines
      [ "Renaming Rule:",
        "Predicate: " <> show predicate,
        show name
      ]

-- | A list of rules for renaming files.
newtype RenamingSchema = RenamingSchema (NonEmptyList RenamingRule)
  deriving (Eq, Generic)
  deriving newtype (FromDhall)

instance Show RenamingSchema where
  show (RenamingSchema fs) =
    unlines $
      "Renaming Schema:"
        : fmap (unlines . fmap ("  " ++) . lines . show) (toList fs)

-- | Convert a `Separator` to its textual representation
sepToText :: Separator -> Text
sepToText Whitespace = " "
sepToText (Keyword t) = safeText t

-- | Check that a file name doesn't conflict with a separator.
checkName :: (Text, Text) -> Text -> Either [OutputFormatError] ()
checkName (p, t) n
  | T.all isSpace t && T.any isSpace n =
    Left [InvalidFileNameForOutputFormat n (p, t)]
  | t `T.isInfixOf` n =
    Left [InvalidFileNameForOutputFormat n (p, t)]
  | otherwise = Right ()

-- | Check that all `Tags` are compatible with separators in an `OutputFormat`
checkTags :: OutputFormat -> Tags -> Validation [OutputFormatError] ()
checkTags (OutputFormat _ open _ tagSep tvF _ close) ts =
  traverse_
    (tagsSepCollision ts)
    $ [ ("opening", open),
        ("tag", tagSep),
        ("closing", close)
      ]
      ++ maybe
        []
        ( \(OutputTaggedValueFormat _ pre tv post _) ->
            maybe [] (\t -> [("pre-tagged value", t)]) pre
              ++ ("tagged value", tv)
                : maybe [] (\t -> [("post-tagged value", t)]) post
        )
        tvF

-- | Check that a tagged value plus the next separator doesn't lead to a
--   separator occurring too early, e.g. @t=@ @==@ @v@ being parsed as
--   @t@ @t==@ @=v@.
checkTrailingSeps :: OutputFormat -> Tags -> Validation [OutputFormatError] ()
checkTrailingSeps (OutputFormat _ _ _ tagSep tvF _ close) =
  traverse_ (trailingSepCollision (safeText tagSep) tvF (safeText close)) . M.assocs . tagMap

-- | Check a tag for all possible trailing collisions.
trailingSepCollision :: Text -> Maybe OutputTaggedValueFormat -> Text -> (Tag, TagValue) -> Validation [OutputFormatError] ()
trailingSepCollision tagSep _ _ (Tag (safeText -> t), _)
  | T.breakOn tagSep (t <> tagSep) /= (t, tagSep) = Failure [InvalidTagForOutputFormat t ("tag", tagSep)]
trailingSepCollision _ _ close (Tag (safeText -> t), Present)
  | T.breakOn close (t <> close) /= (t, close) = Failure [InvalidTagForOutputFormat t ("closing", close)]
trailingSepCollision tagSep _ close (Tag (safeText -> t), Value (safeText -> v))
  | T.breakOn tagSep (v <> tagSep) /= (v, tagSep) = Failure [InvalidTaggedValueForOutputFormat t v ("tag", tagSep)]
  | T.breakOn close (v <> close) /= (v, close) = Failure [InvalidTaggedValueForOutputFormat t v ("closing", close)]
trailingSepCollision _ (Just (OutputTaggedValueFormat TagBeforeValue _ (safeText -> tv) _ _)) _ (Tag (safeText -> t), Value (safeText -> v))
  | T.breakOn tv (t <> tv) /= (t, tv) = Failure [InvalidTaggedValueForOutputFormat t v ("tagged value", tv)]
trailingSepCollision _ (Just (OutputTaggedValueFormat ValueBeforeTag _ (safeText -> tv) _ _)) _ (Tag (safeText -> t), Value (safeText -> v))
  | T.breakOn tv (v <> tv) /= (v, tv) = Failure [InvalidTaggedValueForOutputFormat t v ("tagged value", tv)]
trailingSepCollision _ (Just (OutputTaggedValueFormat TagBeforeValue _ _ (Just (safeText -> post)) _)) _ (Tag (safeText -> t), Value (safeText -> v))
  | T.breakOn post (v <> post) /= (v, post) = Failure [InvalidTaggedValueForOutputFormat t v ("post-tagged value", post)]
trailingSepCollision _ (Just (OutputTaggedValueFormat ValueBeforeTag _ _ (Just (safeText -> post)) _)) _ (Tag (safeText -> t), Value (safeText -> v))
  | T.breakOn post (t <> post) /= (t, post) = Failure [InvalidTaggedValueForOutputFormat t v ("post-tagged value", post)]
trailingSepCollision _ _ _ _ = Success ()

-- | Check `Tags` for compatibility with a given separator.
tagsSepCollision :: Tags -> (Text, SafeText) -> Validation [OutputFormatError] ()
tagsSepCollision ts (p, safeText -> s) =
  traverse_ check . M.assocs . tagMap $ ts
  where
    check
      | T.all isSpace s = checkTagWhitespace (p, s)
      | otherwise = checkTagKeyword (p, s)

-- | Check that a `Tag` and `TagValue` do not contain whitespace that would
--   confuse the input parser.
checkTagWhitespace :: (Text, Text) -> (Tag, TagValue) -> Validation [OutputFormatError] ()
checkTagWhitespace s (Tag (safeText -> t), _)
  | T.any isSpace t = Failure [InvalidTagForOutputFormat t s]
checkTagWhitespace s (Tag (safeText -> t), Value (safeText -> v))
  | T.any isSpace v = Failure [InvalidTaggedValueForOutputFormat t v s]
checkTagWhitespace _ _ = Success ()

-- | Check that a `Tag` and `TagValue` do not contain a keyword that would
--   confuse the input parser.
checkTagKeyword :: (Text, Text) -> (Tag, TagValue) -> Validation [OutputFormatError] ()
checkTagKeyword (p, k) (Tag (safeText -> t), _)
  | k `T.isInfixOf` t = Failure [InvalidTagForOutputFormat t (p, k)]
checkTagKeyword (p, k) (Tag (safeText -> t), Value (safeText -> v))
  | k `T.isInfixOf` v = Failure [InvalidTaggedValueForOutputFormat t v (p, k)]
checkTagKeyword _ _ = Success ()

-- | Given a file's information and a duplicate number (i.e. collided name
--   number) if relevant, rename it according to a schema.
nameBySchema :: RenamingSchema -> Maybe Natural -> FileInfo -> Either RenamingError Text
nameBySchema s n f = case findRenamingRule f s of
  Nothing -> Left $ NoRenamingMatch f
  Just r ->
    validationToEither . first (NameTemplateErrors f)
      . translateFileName f n
      . (name :: RenamingRule -> FileName)
      $ r

-- | Return the first applicable `RenamingRule` for a file (if one exists).
findRenamingRule :: FileInfo -> RenamingSchema -> Maybe RenamingRule
findRenamingRule FileInfo {fileTags} (RenamingSchema rules) =
  go $ toList rules
  where
    go [] = Nothing
    go (r : rs)
      | checkPredicate fileTags ((predicate :: RenamingRule -> Predicate) r) =
        Just r
      | otherwise = go rs

-- | Given an `InputFormat`, possibly an `OutputFormat`, a file's info, and
--   possibly a new file name, write out the full file name for the output file.
writeFileName :: InputFormat -> Maybe OutputFormat -> FileInfo -> Maybe Text -> Either [OutputFormatError] Text
writeFileName inF Nothing f newName =
  case (tagOrder :: InputFormat -> Order) inF of
    NameTagsExtension ->
      case checkName ("opening", sepToText $ (openingSeparator :: InputFormat -> Separator) inF) n of
        Right () ->
          Right $ T.concat [n, ts, ext]
        Left e -> Left e
    TagsNameExtension ->
      case checkName ("closing", sepToText $ (closingSeparator :: InputFormat -> Separator) inF) n of
        Right () ->
          Right $ T.concat [ts, n, ext]
        Left e -> Left e
  where
    n = fromMaybe (safeText $ originalName f) newName
    ts = safeText . rawTags $ f
    ext = maybe "" safeText $ extension f
writeFileName _ (Just outF@(OutputFormat ord (safeText -> open) leading (safeText -> sep) tvF trailing (safeText -> close))) f newName = do
  validationToEither (checkTags outF tags)
  validationToEither (checkTrailingSeps outF tags)
  writtenTags <-
    validationToEither (traverse (writeTag tvF) . M.toList . tagMap $ tags)
  ( \ts ->
      validationToEither . checkCombinedSeps outF tags $
        T.concat
          [ if leading then sep else "",
            T.intercalate sep ts,
            if trailing then sep else ""
          ]
    )
    writtenTags
  ( \ts ->
      case ord of
        NameTagsExtension ->
          T.concat
            [ n,
              open,
              if leading then sep else "",
              T.intercalate sep ts,
              if trailing then sep else "",
              close,
              ext
            ]
            <$ checkName ("opening", open) n
        TagsNameExtension ->
          T.concat
            [ open,
              if leading then sep else "",
              T.intercalate sep ts,
              if trailing then sep else "",
              close,
              n,
              ext
            ]
            <$ checkName ("closing", close) n
    )
    writtenTags
  where
    n = fromMaybe (safeText $ originalName f) newName
    ext = maybe "" safeText $ extension f
    tags = fileTags f

-- | Check to confirm that the output tags have the expected number of
--   separators, i.e. that there haven't been combinatorial effects from
--   adjacent tags that will lead to parsing errors.
checkCombinedSeps :: OutputFormat -> Tags -> Text -> Validation [OutputFormatError] ()
checkCombinedSeps
  (OutputFormat _ _ lead (safeText -> tSep) tvF trail (safeText -> close))
  (tagMap -> ts)
  raw =
    traverse_ check $
      [(("tag", tSep), nTagSeps), (("close", close), 0)] ++ tvChecks
    where
      check (s, n) = checkSepN s n
      nTagSeps =
        intToNatural . max 0 $ M.size ts - 1 + bool 0 1 lead + bool 0 1 trail
      tvChecks = case tvF of
        Nothing -> []
        Just (OutputTaggedValueFormat _ pre tv post _) ->
          (("tagged value", safeText tv), numTVs)
            : maybe [] (\s -> [(("pre-tagged value", safeText s), numTVs)]) pre
            ++ maybe [] (\s -> [(("post-tagged value", safeText s), numTVs)]) post
          where
            numTVs = intToNatural . M.size . M.filter (Present /=) $ ts
      checkSepN (p, s) n
        | intToNatural (T.count s raw) == n = Success ()
        | otherwise = Failure [InvalidTagSeparatorCombination (p, s) n raw]

-- | Write out a `Tag` and `TagValue` in a given output tagged value format, if
--   possible.
writeTag :: Maybe OutputTaggedValueFormat -> (Tag, TagValue) -> Validation [OutputFormatError] Text
writeTag Nothing (Tag (safeText -> t), val) =
  case val of
    Value (safeText -> v) ->
      Failure [TaggedValueWhenOutputForbidsThem t v]
    Present ->
      Success t
writeTag (Just f) (Tag (safeText -> t), val) =
  case f of
    OutputTaggedValueFormat ord pre sep post pureAllowed ->
      case val of
        Present ->
          if pureAllowed
            then Success t
            else Failure [NonTaggedValueWhenOutputForbidsThem t]
        Value (safeText -> v) ->
          Success $ case ord of
            TagBeforeValue ->
              T.concat [maybe "" safeText pre, t, safeText sep, v, maybe "" safeText post]
            ValueBeforeTag ->
              T.concat [maybe "" safeText pre, v, safeText sep, t, maybe "" safeText post]
