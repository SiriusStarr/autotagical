{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Sorting
-- Description : Sorting files to folders
--
-- Contains all necessary types for file sorting to folder schemas and their
-- Dhall instances, as well as the functions to do the actual sorting, given
-- file info.
module Sorting
  ( -- * Sorting
    SortingError (..),
    SortingSchema (..),
    sortBySchema,

    -- * Internal For Testing
    sortToFolder,
    Folder (..),
  )
where

import Data.Bifunctor (first)
import Data.Either.Validation (validationToEither)
import qualified Data.Fix as Fix
import qualified Data.Functor.Foldable as Foldable
import qualified Data.Functor.Foldable.TH as TH
import qualified Data.Text as T
import Dhall (FromDhall, autoWith)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import NameTemplate (FolderName, NameTemplateError, translateFolderName)
import Parser (FileInfo (..))
import Predicate (Predicate, checkPredicate)
import SafeType (NonEmptyList)
import System.FilePath (FilePath)
import qualified System.FilePath as FP
import Tag (Tags (..))

-- * Sorting

-- | Specify the way in which a file failed to be sorted.
data SortingError
  = NameTemplateErrors {file :: FileInfo, errors :: [NameTemplateError]}
  | NoSortingMatch {file :: FileInfo}
  deriving (Eq)

instance Show SortingError where
  show NoSortingMatch {file} = "No sorting match was found (i.e. no top-level folder predicates evaluated to True) for the following file:\n" <> show file
  show NameTemplateErrors {file, errors} =
    unlines $
      [ "A sorting match was found for the file:",
        show file,
        "but the following errors occurred while translating name templates for the folders:"
      ]
        ++ fmap show errors

-- | A folder for sorting files into, with a name template, predicate for
--   filtering, and a possible list of subfolders.
data Folder
  = Folder
      { name :: FolderName,
        predicate :: Predicate,
        subfolders :: [Folder]
      }
  deriving (Eq, Generic)

instance Show Folder where
  show Folder {name, predicate, subfolders} =
    unlines $
      [ "Folder:",
        "Predicate: " <> show predicate,
        show name,
        "Subfolders:"
      ]
        ++ fmap (unlines . fmap ("  " ++) . lines . show) subfolders

TH.makeBaseFunctor ''Folder

deriving instance Generic (FolderF a)

deriving instance FromDhall a => FromDhall (FolderF a)

instance FromDhall Folder where
  autoWith opts = Fix.cata Foldable.embed <$> autoWith opts

-- | A list of rules for sorting files.
newtype SortingSchema = SortingSchema (NonEmptyList Folder)
  deriving (Eq, Generic)
  deriving newtype (FromDhall)

instance Show SortingSchema where
  show (SortingSchema fs) =
    unlines $
      "Sorting Schema:"
        : fmap (unlines . fmap ("  " ++) . lines . show) (toList fs)

-- | Given a file's information, sort it according to a schema.
sortBySchema :: SortingSchema -> FileInfo -> Either SortingError FilePath
sortBySchema (SortingSchema folders) i =
  case sortToFolder (fileTags i) (toList folders) of
    Nothing -> Left $ NoSortingMatch i
    Just fs ->
      validationToEither $
        FP.joinPath . map T.unpack
          <$> first
            (NameTemplateErrors i)
            ( traverse
                (translateFolderName i . name)
                fs
            )

-- | Given a file's tags and a sorting schema, determine where it goes
sortToFolder :: Tags -> [Folder] -> Maybe [Folder]
sortToFolder _ [] = Just []
sortToFolder ts folders =
  case go folders of
    Nothing ->
      Nothing
    Just f -> case sortToFolder ts (subfolders f) of
      Just fs -> Just $ f : fs
      Nothing -> Just [f]
  where
    go [] = Nothing
    go (f : fs)
      | checkPredicate ts (predicate f) = Just f
      | otherwise = go fs
