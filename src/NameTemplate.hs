{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : NameTemplate
-- Description : "Format strings" for file and folder names
--
-- Contains all necessary types for file and folder names and their Dhall
-- instances, as well as the functions to translate them given provided parsed
-- file information.
module NameTemplate
  ( -- * Translation Errors
    NameTemplateError (..),

    -- * Folder
    translateFolderName,
    FolderName (..),

    -- * File
    translateFileName,
    FileName (..),

    -- * Internal For Testing
    Case (..),
    MonthFormat (..),
    YearFormat (..),
    LatestYear (..),
    Interpret (..),
    FolderNameComponent (..),
    translateFolderComponent,
    translateFolderComponents,
    FileNameComponent (..),
    translateFileComponent,
    translateFileComponents,
  )
where

import Data.Either.Validation (Validation (..))
import qualified Data.Fix as Fix
import Data.Foldable (traverse_)
import qualified Data.Functor.Foldable as Foldable
import qualified Data.Functor.Foldable.TH as TH
import Data.List (genericIndex)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Manipulate
  ( toCamel,
    toOrdinal,
    toPascal,
    toSnake,
    toSpinal,
    toTitle,
    toTrain,
  )
import Dhall (Decoder (..), FromDhall, autoWith, extractError)
import qualified Dhall as D
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Parser (FileInfo (..))
import Predicate (Predicate, checkPredicate)
import SafeType (NonEmptyList, SafeText, safeText)
import Tag (Tag (..), TagValue (..), Tags (..))
import Text.Read (readEither)

-- * Translation Errors

-- | Specify the way in which a `FileName` or `FolderName`
--   folder name component failed to be translated.
--
--   [`InterpretNumberOnNonNumber`] `FolderInterpret` or `FileInterpret` was
--     called on a value that could not be interpreted as a natural number.
--   [`NullNameTemplate`] The net output of a `FileName` or `FolderName` after
--     translation was completely empty.
--   [`NumberAsMonthOutOfRange`] Interpret `NumberAsMonth` was called on a
--     value that was a natural number, but was 13 or more.
--   [`NumberAsYearOutOfRange`] Interpret `NumberAsYear` was called on a value
--     that was a natural number but was 100-999 or 10,000+ (i.e. not 1, 2, or 4
--     digits).
--   [`TagValueWithoutTag`] `FolderTagValue` or `FileTagValue` requested the
--     value of a tag that was not on the file.
data NameTemplateError
  = InterpretNumberOnNonNumber
      { component :: Text,
        errorMessage :: Text
      }
  | NullNameTemplate {template :: Text}
  | NumberAsMonthOutOfRange
      { component :: Text,
        number :: Natural
      }
  | NumberAsYearOutOfRange
      { component :: Text,
        number :: Natural
      }
  | TagValueWithoutTag {requestedTag :: Tag}
  deriving (Eq)

instance Show NameTemplateError where
  show InterpretNumberOnNonNumber {component, errorMessage} =
    concat
      [ "\"Interpret as\" was called on the following component: ",
        T.unpack component,
        ", but attempting to interpret that as a number failed with the following error:\n",
        T.unpack errorMessage
      ]
  show NullNameTemplate {template} =
    "The file or folder name template evaluated to an empty string!  The template was: " <> T.unpack template
  show NumberAsMonthOutOfRange {component, number} =
    concat
      [ "\"Interpret as month\" was called on the following component: ",
        T.unpack component,
        ", but while that evaluated to the number ",
        show number,
        " this was out of range for a month (i.e. 13 or greater)."
      ]
  show NumberAsYearOutOfRange {component, number} =
    concat
      [ "\"Interpret as year\" was called on the following component: ",
        T.unpack component,
        ", but while that evaluated to the number ",
        show number,
        " this was out of range for a year (i.e. was 3 or 5+ digits).  Only years within the range 0-99 or 1000-9999 are supported."
      ]
  show TagValueWithoutTag {requestedTag} =
    "The tag value for the tag "
      <> show requestedTag
      <> " was requested, but that tag was not on the file!"

-- * Folder and File Names

-- ** Common Components

-- | The case to format a name template component as, given the FormatAs
--   component.  Examples of each are given below:
--
-- [`AsCamelCase`] @asCamelCase@
-- [`AsPascalCase`] @AsPascalCase@
-- [`AsSnakeCase`] @as_snake_case@
-- [`AsSpinalCase`] @as-spinal-case@
-- [`AsTitleCase`] @As Title Case@
-- [`AsTrainCase`] @As-Train-Case@
data Case
  = AsCamelCase
  | AsPascalCase
  | AsSnakeCase
  | AsSpinalCase
  | AsTitleCase
  | AsTrainCase
  deriving (Eq, FromDhall, Generic)

instance Show Case where
  show AsCamelCase = "camelCase"
  show AsPascalCase = "PascalCase"
  show AsSnakeCase = "snake_case"
  show AsSpinalCase = "spinal-case"
  show AsTitleCase = "Title Case"
  show AsTrainCase = "Train-Case"

-- | Format to write a name template component interpreted as a month as.
--   Examples are given below:
--
-- [`MonthName`] @January@
-- [`OneDigitMonth`] @1@
-- [`ThreeLetterMonth`] @Jan@
-- [`TwoDigitMonth`] @01@
data MonthFormat
  = MonthName
  | OneDigitMonth
  | ThreeLetterMonth
  | TwoDigitMonth
  deriving (Eq, FromDhall, Generic)

instance Show MonthFormat where
  show MonthName = "January"
  show OneDigitMonth = "1"
  show ThreeLetterMonth = "Jan"
  show TwoDigitMonth = "01"

-- | Format to write a name template component interpreted as a year as.
--   Examples are given below:
--
-- [`FourDigitYear`] @1995@
-- [`TwoDigitYear`] @95@
data YearFormat
  = FourDigitYear
  | TwoDigitYear
  deriving (Eq, FromDhall, Generic)

instance Show YearFormat where
  show FourDigitYear = "1995"
  show TwoDigitYear = "95"

-- | The year on which to split interpretation of a two-digit year.
newtype LatestYear = LatestYear Natural deriving (Eq, Generic)

-- | `FromDhall` instance for `LatestYear` that ensures it is in valid range.
instance FromDhall LatestYear where
  autoWith _ = validateLatestYear D.natural
    where
      validateLatestYear (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success n
              | n < 1099 || n > 9999 ->
                extractError "Specified latest year for interpreting a number as a year must be greater than 999, so that 2 digit years can be interpreted.  For example, given a latest year of \"2022\", the numbers \"21\" and \"22\" will be interpreted as \"2021\" and \"2022\", while \"23\" will be interpreted as \"1923\"."
              | otherwise ->
                Success $ LatestYear n
            Failure f -> Failure f

instance Show LatestYear where
  show (LatestYear n) = "Latest Year: " <> show n

-- | Specify how to interpret a name template component.  `NumberAsOrdinal`
--   converts `1` to `1st`.
data Interpret
  = NumberAsOrdinal
  | NumberAsMonth MonthFormat
  | NumberAsYear {latestYear :: LatestYear, yearFormat :: YearFormat}
  deriving (Eq, FromDhall, Generic)

instance Show Interpret where
  show NumberAsOrdinal = "as an ordinal number"
  show (NumberAsMonth f) = "as a Month: " <> show f
  show NumberAsYear {latestYear, yearFormat} =
    concat
      [ "as a year: ",
        show yearFormat,
        " with ",
        show latestYear
      ]

-- ** Folder Names

-- | A part of a `FolderName` template that will be converted to text based on
--   file information.
data FolderNameComponent
  = FolderFormatAs
      { components :: NonEmptyList FolderNameComponent,
        formatCase :: Case
      }
  | FolderIfThenElse
      { predicate :: Predicate,
        trueComponents :: [FolderNameComponent],
        falseComponents :: [FolderNameComponent]
      }
  | FolderInterpret
      { component :: FolderNameComponent,
        interpretAs :: Interpret
      }
  | FolderOriginalName
  | FolderTagValue Tag
  | FolderTextLiteral SafeText
  deriving (Eq, Generic)

instance Show FolderNameComponent where
  show FolderFormatAs {components, formatCase} =
    concat
      [ "Format ",
        show components,
        " as ",
        show formatCase
      ]
  show FolderIfThenElse {predicate, trueComponents, falseComponents} =
    concat
      [ "If ",
        show predicate,
        " then ",
        show trueComponents,
        " else ",
        show falseComponents
      ]
  show FolderInterpret {component, interpretAs} =
    concat
      [ "Interpret ",
        show component,
        " ",
        show interpretAs
      ]
  show FolderOriginalName = "Original file name"
  show (FolderTagValue t) = "Value of tag " <> show t
  show (FolderTextLiteral t) = show t

TH.makeBaseFunctor ''FolderNameComponent

deriving instance Generic (FolderNameComponentF a)

deriving instance FromDhall a => FromDhall (FolderNameComponentF a)

-- | Ensure that `FolderIfThenElse` doesn't contain no components whatsoever.
--   This shouldn't be achievable through the "exposed" Dhall interface.
validateFolderNameComponent :: FolderNameComponent -> Validation Text ()
validateFolderNameComponent (FolderIfThenElse _ [] []) =
  Failure "The IfThenElse folder name component must have at least one component, i.e. at least one of the true components or false components must be non-empty.  This should not be achievable using the normal Dhall interface, so if you weren't messing around with internal Dhall types, please report it as a bug."
validateFolderNameComponent (FolderFormatAs cs _) =
  traverse_ validateFolderNameComponent cs
validateFolderNameComponent (FolderIfThenElse _ c1s c2s) =
  traverse_ validateFolderNameComponent $ c1s ++ c2s
validateFolderNameComponent (FolderInterpret c _) =
  validateFolderNameComponent c
validateFolderNameComponent _ = Success ()

-- | `FromDhall` instance that validates no empty `FolderIfThenElse`'s.
instance FromDhall FolderNameComponent where
  autoWith opts = validate $ Fix.cata Foldable.embed <$> autoWith opts
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success p -> case validateFolderNameComponent p of
              Failure err -> extractError err
              Success () -> Success p
            Failure f -> Failure f

-- | A complete folder name template, consisting of one or more
--   `FolderNameComponent`s and possibly a separator.
data FolderName
  = FolderName
      { components :: NonEmptyList FolderNameComponent,
        separator :: Maybe SafeText
      }
  deriving (Eq, FromDhall, Generic)

instance Show FolderName where
  show FolderName {components, separator} =
    concat $
      ["Folder Name: ", show components]
        ++ maybe [] (\s -> [", separated by ", show s]) separator

-- ** File Names

-- | A part of a `FileName` template that will be converted to text based on
--   file information.
data FileNameComponent
  = FileDuplicateNumber
  | FileFormatAs
      { components :: NonEmptyList FileNameComponent,
        formatCase :: Case
      }
  | FileIfDuplicate (NonEmptyList FileNameComponent)
  | FileIfThenElse
      { predicate :: Predicate,
        trueComponents :: [FileNameComponent],
        falseComponents :: [FileNameComponent]
      }
  | FileInterpret
      { component :: FileNameComponent,
        interpretAs :: Interpret
      }
  | FileOriginalName
  | FileTagValue Tag
  | FileTextLiteral SafeText
  deriving (Eq, Generic)

instance Show FileNameComponent where
  show FileDuplicateNumber = "Duplicate number"
  show FileFormatAs {components, formatCase} =
    concat
      [ "Format ",
        show components,
        " as ",
        show formatCase
      ]
  show (FileIfDuplicate cs) = "If duplicate: " <> show cs
  show FileIfThenElse {predicate, trueComponents, falseComponents} =
    concat
      [ "If ",
        show predicate,
        " then ",
        show trueComponents,
        " else ",
        show falseComponents
      ]
  show FileInterpret {component, interpretAs} =
    concat
      [ "Interpret ",
        show component,
        " ",
        show interpretAs
      ]
  show FileOriginalName = "Original file name"
  show (FileTagValue t) = "Value of tag " <> show t
  show (FileTextLiteral t) = show t

TH.makeBaseFunctor ''FileNameComponent

deriving instance Generic (FileNameComponentF a)

deriving instance FromDhall a => FromDhall (FileNameComponentF a)

-- | Ensure that `FileIfThenElse` doesn't contain no components whatsoever.
--   This shouldn't be achievable through the "exposed" Dhall interface.
validateFileNameComponent :: FileNameComponent -> Validation Text ()
validateFileNameComponent (FileIfThenElse _ [] []) =
  Failure "The IfThenElse file name component must have at least one component, i.e. at least one of the true components or false components must be non-empty, but I encountered one anyways.  This should not be achievable using the normal Dhall interface, so if you weren't messing around with internal Dhall types, please report it as a bug."
validateFileNameComponent (FileFormatAs cs _) =
  traverse_ validateFileNameComponent cs
validateFileNameComponent (FileIfThenElse _ c1s c2s) =
  traverse_ validateFileNameComponent $ c1s ++ c2s
validateFileNameComponent (FileInterpret c _) = validateFileNameComponent c
validateFileNameComponent _ = Success ()

-- | `FromDhall` instance that validates no empty `FileIfThenElse`'s.
instance FromDhall FileNameComponent where
  autoWith opts = validate $ Fix.cata Foldable.embed <$> autoWith opts
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success p -> case validateFileNameComponent p of
              Failure err -> extractError err
              Success () -> Success p
            Failure f -> Failure f

-- | A complete file name template, consisting of one or more
--   `FileNameComponent`s and possibly a separator.
data FileName
  = FileName
      { components :: NonEmptyList FileNameComponent,
        separator :: Maybe SafeText
      }
  deriving (Eq, FromDhall, Generic)

instance Show FileName where
  show FileName {components, separator} =
    concat $
      ["File Name: ", show components]
        ++ maybe [] (\s -> [", separated by ", show s]) separator

-- * Common Functions

-- | Given a `MonthFormat`, try to convert a `Natural` into that format or fail.
toMonth :: MonthFormat -> Natural -> Maybe Text
toMonth f i
  | i == 0 || i > 12 = Nothing
  | otherwise = case f of
    MonthName ->
      Just $
        [ "January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December"
        ]
          `genericIndex` (i - 1)
    ThreeLetterMonth ->
      Just $
        [ "Jan",
          "Feb",
          "Mar",
          "Apr",
          "May",
          "Jun",
          "Jul",
          "Aug",
          "Sep",
          "Oct",
          "Nov",
          "Dec"
        ]
          `genericIndex` (i - 1)
    TwoDigitMonth ->
      Just $
        ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
          `genericIndex` (i - 1)
    OneDigitMonth -> Just . T.pack . show $ i

-- | Given a `YearFormat`, the latest year to consider for 2 digit years, and a
--   `Natural`, try to output the number in that format or fail.
toYear :: YearFormat -> LatestYear -> Natural -> Maybe Text
toYear f (LatestYear latest) i =
  case f of
    FourDigitYear -> T.pack . show <$> as4
    TwoDigitYear -> T.pack . show . (`rem` 100) <$> as4
  where
    as4
      | i >= 1000 && i <= 9999 = Just i
      | i > y && i <= 99 = Just $ (m - 1) * 100 + i
      | i <= y = Just $ m * 100 + i
      | otherwise = Nothing
      where
        (m, y) = latest `quotRem` 100

-- * Folder

-- | Given `FileInfo`, a separator, and a `FolderNameComponent`, turn that
--   component into text.
translateFolderComponent ::
  FileInfo ->
  Text ->
  FolderNameComponent ->
  Validation [NameTemplateError] Text
translateFolderComponent f sep (FolderFormatAs cs format) =
  toCase . T.strip . T.intercalate " " . toList
    <$> traverse (translateFolderComponent f sep) cs
  where
    toCase = case format of
      AsTitleCase -> toTitle
      AsCamelCase -> toCamel
      AsPascalCase -> toPascal
      AsSnakeCase -> toSnake
      AsSpinalCase -> toSpinal
      AsTrainCase -> toTrain
translateFolderComponent f sep (FolderIfThenElse p ts fs)
  | checkPredicate (fileTags f) p = translateFolderComponents f sep ts
  | otherwise = translateFolderComponents f sep fs
translateFolderComponent f sep c@(FolderInterpret c' NumberAsOrdinal) =
  case readEither . T.unpack <$> translateFolderComponent f sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> Success $ toOrdinal (n :: Natural)
translateFolderComponent f sep c@(FolderInterpret c' (NumberAsMonth m)) =
  case readEither . T.unpack <$> translateFolderComponent f sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> case toMonth m n of
      Just t -> Success t
      Nothing -> Failure [NumberAsMonthOutOfRange (T.pack $ show c) n]
translateFolderComponent f sep c@(FolderInterpret c' (NumberAsYear y format)) =
  case readEither . T.unpack <$> translateFolderComponent f sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> case toYear format y n of
      Just t -> Success t
      Nothing -> Failure [NumberAsYearOutOfRange (T.pack $ show c) n]
translateFolderComponent f _ FolderOriginalName =
  Success . safeText . originalName $ f
translateFolderComponent f _ (FolderTagValue t) =
  case M.lookup t (tagMap $ fileTags f) of
    Just Present -> Success . safeText . tag $ t
    Just (Value v) -> Success $ safeText v
    Nothing -> Failure [TagValueWithoutTag t]
translateFolderComponent _ _ (FolderTextLiteral t) = Success $ safeText t

-- | Translate multiple components and join non-empty ones by using the
--   specified separator (which may be empty).
translateFolderComponents ::
  FileInfo ->
  Text ->
  [FolderNameComponent] ->
  Validation [NameTemplateError] Text
translateFolderComponents f sep cs =
  T.intercalate sep . filter (not . T.null)
    <$> traverse (translateFolderComponent f sep) cs

-- | Translate a `FolderName`.
translateFolderName ::
  FileInfo ->
  FolderName ->
  Validation [NameTemplateError] Text
translateFolderName f n@(FolderName cs s) =
  case translateFolderComponents f (maybe "" safeText s) (toList cs) of
    Success "" ->
      Failure [NullNameTemplate (T.pack $ show n)]
    v -> v

-- * File

-- | Given file info, a possible duplicate number, a separator, and a
--   `FileNameComponent`, turn that component into text.
translateFileComponent ::
  FileInfo ->
  Maybe Natural ->
  Text ->
  FileNameComponent ->
  Validation [NameTemplateError] Text
translateFileComponent _ d _ FileDuplicateNumber =
  Success $ maybe "" (T.pack . show) d
translateFileComponent f d sep (FileFormatAs cs format) =
  toCase . T.strip . T.intercalate " " . toList
    <$> traverse (translateFileComponent f d sep) cs
  where
    toCase = case format of
      AsTitleCase -> toTitle
      AsCamelCase -> toCamel
      AsPascalCase -> toPascal
      AsSnakeCase -> toSnake
      AsSpinalCase -> toSpinal
      AsTrainCase -> toTrain
translateFileComponent _ Nothing _ (FileIfDuplicate _) = Success ""
translateFileComponent f d sep (FileIfDuplicate cs) =
  translateFileComponents f d sep $ toList cs
translateFileComponent f d sep (FileIfThenElse p ts fs)
  | checkPredicate (fileTags f) p = translateFileComponents f d sep ts
  | otherwise = translateFileComponents f d sep fs
translateFileComponent f d sep c@(FileInterpret c' NumberAsOrdinal) =
  case readEither . T.unpack <$> translateFileComponent f d sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> Success $ toOrdinal (n :: Natural)
translateFileComponent f d sep c@(FileInterpret c' (NumberAsMonth m)) =
  case readEither . T.unpack <$> translateFileComponent f d sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> case toMonth m n of
      Just t -> Success t
      Nothing -> Failure [NumberAsMonthOutOfRange (T.pack $ show c) n]
translateFileComponent f d sep c@(FileInterpret c' (NumberAsYear y format)) =
  case readEither . T.unpack <$> translateFileComponent f d sep c' of
    Failure e -> Failure e
    Success (Left e) ->
      Failure [InterpretNumberOnNonNumber (T.pack $ show c) (T.pack e)]
    Success (Right n) -> case toYear format y n of
      Just t -> Success t
      Nothing -> Failure [NumberAsYearOutOfRange (T.pack $ show c) n]
translateFileComponent f _ _ FileOriginalName =
  Success . safeText . originalName $ f
translateFileComponent f _ _ (FileTagValue t) =
  case M.lookup t (tagMap $ fileTags f) of
    Just Present -> Success . safeText . tag $ t
    Just (Value v) -> Success $ safeText v
    Nothing -> Failure [TagValueWithoutTag t]
translateFileComponent _ _ _ (FileTextLiteral t) = Success $ safeText t

-- | Translate multiple components and join non-empty ones by using the
--   specified separator (which may be empty).
translateFileComponents ::
  FileInfo ->
  Maybe Natural ->
  Text ->
  [FileNameComponent] ->
  Validation [NameTemplateError] Text
translateFileComponents f d sep cs =
  T.intercalate sep . filter (not . T.null)
    <$> traverse (translateFileComponent f d sep) cs

-- | Translate a `FileName`.
translateFileName :: FileInfo -> Maybe Natural -> FileName -> Validation [NameTemplateError] Text
translateFileName f d n@(FileName cs s) =
  case translateFileComponents f d (maybe "" safeText s) (toList cs) of
    Success "" ->
      Failure [NullNameTemplate (T.pack $ show n)]
    v -> v
