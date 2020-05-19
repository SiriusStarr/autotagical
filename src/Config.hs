{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( -- * Config
    AutotagicalConfig (..),
  )
where

import Dhall (FromDhall, autoWith, field, record)
import qualified Dhall as D
import FileHandler (ClobberDestination, GlobPatterns)
import Logging (LogDestination, LogLevel)
import Parser (InputFormat, OutputFormat)
import Renaming (RenamingSchema)
import SafeType (NonEmptyList)
import Sorting (SortingSchema)

-- * Config

-- | The complete settings for a run of Autotagical, as specified by Dhall.
data AutotagicalConfig
  = AutotagicalConfig
      { clobberDestination :: ClobberDestination,
        ignorePatterns :: Maybe GlobPatterns,
        inputFolders :: NonEmptyList FilePath,
        inputFormat :: InputFormat,
        inputPatterns :: GlobPatterns,
        keepCopyInInputFolder :: Bool,
        logDestination :: LogDestination,
        logLevel :: LogLevel,
        outputFolders :: NonEmptyList FilePath,
        outputFormat :: Maybe OutputFormat,
        renaming :: Maybe (GlobPatterns, RenamingSchema),
        sortingSchema :: SortingSchema
      }
  deriving (Eq)

instance Show AutotagicalConfig where
  show
    AutotagicalConfig
      { clobberDestination,
        ignorePatterns,
        inputFolders,
        inputFormat,
        inputPatterns,
        keepCopyInInputFolder,
        logDestination,
        logLevel,
        outputFolders,
        outputFormat,
        renaming,
        sortingSchema
      } =
      concat
        [ "Autotagical Config:",
          "\n\nInput:\nInput Folders: ",
          show inputFolders,
          "\nInput Patterns: ",
          show inputPatterns,
          "\nIgnore Patterns: ",
          show ignorePatterns,
          "\nInput Format:\n",
          show inputFormat,
          "\n\nOutput:\nOutput Folders: ",
          show outputFolders,
          "\nOutput Format: ",
          show outputFormat,
          "\nKeep Copy: ",
          show keepCopyInInputFolder,
          "\nClobber Destination: ",
          show clobberDestination,
          "\n\nLogging:\nLog Level: ",
          show logLevel,
          "\nLog Destination: ",
          show logDestination,
          "\n\nSorting Schema:\n",
          show sortingSchema,
          "\n\nRenaming:"
        ]
        ++ maybe
          " None"
          ( \(ps, s) ->
              concat
                [ "\nRenaming Patterns: ",
                  show ps,
                  "\nRenaming Schema:\n",
                  show s
                ]
          )
          renaming

-- | Custom instance so as to have a nice, nested record in Dhall for renaming
--   without having a record in Haskell.
instance FromDhall AutotagicalConfig where
  autoWith opts =
    record
      ( AutotagicalConfig
          <$> field "clobberDestination" (autoWith opts)
          <*> field "ignorePatterns" (autoWith opts)
          <*> field "inputFolders" (autoWith opts)
          <*> field "inputFormat" (autoWith opts)
          <*> field "inputPatterns" (autoWith opts)
          <*> field "keepCopyInInputFolder" (autoWith opts)
          <*> field "logDestination" (autoWith opts)
          <*> field "logLevel" (autoWith opts)
          <*> field "outputFolders" (autoWith opts)
          <*> field "outputFormat" (autoWith opts)
          <*> field
            "renaming"
            ( D.maybe
                ( record
                    ( (,)
                        <$> field "patterns" (autoWith opts)
                        <*> field "schema" (autoWith opts)
                    )
                )
            )
          <*> field "sortingSchema" (autoWith opts)
      )
