{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : SafeType
-- Description : Guaranteed safe types
--
-- Non-empty sets, lists, and text guaranteed to not contain characters unsafe
-- for file names (e.g. "/").
module SafeType
  ( -- * Text
    SafeText,
    safeText,
    validateText,

    -- * Lists
    NonEmptyList,

    -- * Sets
    NonEmptySet,
    makeNonEmptySet,
    toSet,
  )
where

import Data.Char (isPrint)
import Data.Either.Validation (Validation (..))
import qualified Data.Set as S
import Data.Set (Set)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text (Text)
import Dhall (Decoder (..), FromDhall, autoWith, extractError)
import qualified Dhall as D
import GHC.Exts (IsList (..))

-- * Text

-- | Text that may not be empty or contain "/" (i.e. is safe for use in file
--   and folder names).
newtype SafeText = SafeText Text
  deriving (Eq, Ord)
  deriving newtype (Show)

instance IsString SafeText where
  fromString s = case validateText $ T.pack s of
    Right t -> t
    Left e ->
      error $ "Attempted to make SafeText from invalid string:" <> T.unpack e

-- | Unwrap `SafeText`.
safeText :: SafeText -> Text
safeText (SafeText t) = t

-- | Convert `Text` to `SafeText` or fail with descriptive error message.
validateText :: Text -> Either Text SafeText
validateText t
  | T.null t = Left "Text values cannot be empty."
  | T.isInfixOf "/" t =
    Left $
      "The text value "
        <> T.pack (show t)
        <> " contains the invalid character: '/'\nThis cannot appear in file or folder names and so should not be used."
  | T.any (not . isPrint) t =
    Left $
      T.concat
        [ "Text values can only contain printable Unicode characters (letters, numbers, marks, punctuation, symbols, and spaces).  You tried to use ",
          T.pack (show t),
          ", which contains the following characters: ",
          T.pack (show $ T.filter (not . isPrint) t)
        ]
  | otherwise = Right $ SafeText t

-- | Dhall instance for `SafeText` that ensures it is non-empty and contains no
--   slashes or non-printable characters.
instance FromDhall SafeText where
  autoWith _ = validate D.strictText
    where
      validate :: Decoder Text -> Decoder SafeText
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success t ->
              case validateText t of
                Left err -> extractError err
                Right st -> Success st
            Failure f -> Failure f

-- * Lists

-- | List that may not be empty.  Use this instead of NonEmpty, because that has
--   an established, not very user-friendly Dhall instance.
newtype NonEmptyList a = NonEmptyList [a]
  deriving (Eq, Foldable, Functor, Ord, Traversable)
  deriving newtype (Show)

instance IsList (NonEmptyList a) where
  type Item (NonEmptyList a) = a

  fromList [] = error "Attempted to make NonEmptyList from empty list."
  fromList as = NonEmptyList as

  toList (NonEmptyList as) = as

-- | Dhall instance for NonEmptyList that uses a list for user convenience
--   instead of having to mess with "tuples."
instance FromDhall a => FromDhall (NonEmptyList a) where
  autoWith opts = validateList (D.list (autoWith opts))
    where
      validateList :: Decoder [a] -> Decoder (NonEmptyList a)
      validateList (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success [] ->
              extractError "Lists of predicates, tags, and the like must be non-empty!"
            Success l ->
              Success $ fromList l
            Failure f -> Failure f

-- * Sets

-- | A `Set` with at least one element at the time of creation, wrapped for a
--   `FromDhall` instance.
newtype NonEmptySet a = NonEmptySet {set :: Set a}
  deriving (Eq)

instance Show a => Show (NonEmptySet a) where
  show = show . S.toAscList . toSet

-- | Dhall instance for `NonEmptySet`.  Ignores duplicates.
instance (FromDhall a, Ord a) => FromDhall (NonEmptySet a) where
  autoWith opts = makeNonEmptySet <$> D.autoWith opts

-- | Create a `NonEmptySet` from a `NonEmptyList`.
makeNonEmptySet :: Ord a => NonEmptyList a -> NonEmptySet a
makeNonEmptySet = NonEmptySet . S.fromList . toList

-- | Unwrap a `NonEmptySet`.
toSet :: NonEmptySet a -> Set a
toSet = set
