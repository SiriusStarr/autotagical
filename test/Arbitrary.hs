{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Arbitrary
  (
  )
where

import Data.Either (fromRight, isRight)
import Data.Either.Validation (validationToEither)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.Natural (intToNatural)
import Generic.Random
  ( (%),
    genericArbitrary,
    genericArbitraryRec,
    genericArbitraryU,
  )
import NameTemplate
  ( Case,
    FileName,
    FileNameComponent,
    FolderName,
    FolderNameComponent,
    Interpret,
    LatestYear (..),
    MonthFormat,
    YearFormat,
  )
import Parser
  ( FileInfo (..),
    InputFormat,
    Order,
    OutputFormat,
    OutputTaggedValueFormat,
    Separator,
    SeparatorRequirement,
    TagValueOrder,
    TaggedValueFormat,
    validateInputFormat,
    validateOutputFormat,
  )
import Predicate (Bound, Predicate, validatePredicate)
import Renaming (RenamingRule, RenamingSchema)
import SafeType
  ( NonEmptyList,
    NonEmptySet,
    SafeText,
    makeNonEmptySet,
    validateText,
  )
import Sorting (Folder)
import System.FilePath (extSeparator, isExtSeparator, isPathSeparator)
import Tag (Tag (..), TagGroup (..), TagValue, Tags (..))
import Test.QuickCheck hiding (NonEmptyList)
import Test.QuickCheck.Instances.Natural ()

-- * SafeType

genSafeText :: Gen SafeText
genSafeText =
  fromRight undefined . validateText . T.pack
    <$> listOf1 arbitraryPrintableChar `suchThat` (not . any isPathSeparator)

instance Arbitrary SafeText where
  arbitrary = genSafeText

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = fromList <$> listOf1 arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySet a) where
  arbitrary = makeNonEmptySet <$> arbitrary

-- * Tag

instance Arbitrary Tag where
  arbitrary = Tag <$> genSafeText

instance Arbitrary TagValue where
  arbitrary = genericArbitraryU

instance Arbitrary Tags where
  arbitrary = Tags <$> arbitrary `suchThat` (not . M.null)

instance Arbitrary TagGroup where
  arbitrary = TagGroup <$> arbitrary

-- * Predicate

instance Arbitrary Bound where
  arbitrary = genericArbitraryU

instance Arbitrary Predicate where
  arbitrary =
    genericArbitraryRec (10 % 1 % 10 % 10 % 2 % ())
      `suchThat` (isRight . validationToEither . validatePredicate)

-- * Parser

instance Arbitrary Order where
  arbitrary = genericArbitraryU

instance Arbitrary TagValueOrder where
  arbitrary = genericArbitraryU

instance Arbitrary SeparatorRequirement where
  arbitrary = genericArbitraryU

instance Arbitrary Separator where
  -- Make Whitespace less likely than Keyword so fewer conflicting formats are
  -- generated.
  arbitrary = genericArbitrary (1 % 4 % ())

instance Arbitrary TaggedValueFormat where
  arbitrary = genericArbitraryU

instance Arbitrary InputFormat where
  arbitrary =
    genericArbitraryU
      `suchThat` (isRight . validationToEither . validateInputFormat)

instance Arbitrary OutputTaggedValueFormat where
  arbitrary = genericArbitraryU

instance Arbitrary OutputFormat where
  arbitrary =
    genericArbitraryU
      `suchThat` (isRight . validationToEither . validateOutputFormat)

-- | Make extensions (or the lack thereof).
genExtensions :: Gen (Maybe SafeText)
genExtensions =
  ( \es ->
      if null es
        then Nothing
        else
          Just . fromRight undefined . validateText . T.concat
            . fmap (T.cons extSeparator)
            $ es
  )
    <$> listOf
      ( T.pack <$> listOf1 arbitraryPrintableChar
          `suchThat` ( \cs ->
                         not (any isPathSeparator cs)
                           && not (any isExtSeparator cs)
                     )
      )

instance Arbitrary FileInfo where
  arbitrary =
    FileInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> genExtensions

-- * NameTemplate

instance Arbitrary Case where
  arbitrary = genericArbitraryU

instance Arbitrary MonthFormat where
  arbitrary = genericArbitraryU

instance Arbitrary YearFormat where
  arbitrary = genericArbitraryU

instance Arbitrary LatestYear where
  arbitrary = LatestYear . intToNatural <$> choose (1099, 9999)

instance Arbitrary Interpret where
  arbitrary = genericArbitraryU

instance Arbitrary FolderNameComponent where
  arbitrary = genericArbitraryRec (2 % 2 % 4 % 10 % 10 % 20 % ())

instance Arbitrary FolderName where
  arbitrary = genericArbitraryU

instance Arbitrary FileNameComponent where
  arbitrary = genericArbitraryRec (10 % 1 % 1 % 1 % 4 % 10 % 10 % 20 % ())

instance Arbitrary FileName where
  arbitrary = genericArbitraryU

-- * Sorting

instance Arbitrary Folder where
  arbitrary = genericArbitraryU

instance Arbitrary RenamingRule where
  arbitrary = genericArbitraryU

instance Arbitrary RenamingSchema where
  arbitrary = genericArbitraryU
