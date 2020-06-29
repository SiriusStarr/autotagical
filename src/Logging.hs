{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Logging
-- Description : Logging for all steps.
--
-- Configuration settings for log level and destination as well as logging
-- functions for each step of the process.  The only logging outside of this
-- module is in the `FileHandler.copyFileToPath` function.
module Logging
  ( LogLevel (..),
    LogDestination (..),
    logIgnored,
    logLoaded,
    logParsingFailures,
    logParsingSuccesses,
    logSortingFailures,
    logSortingSuccesses,
    logRenamingFailures,
    logMoves,
    logPlannedMoves,
    logRenamingSuccesses,
  )
where

import Control.Logging hiding (LogLevel (..))
import Control.Monad (unless)
import Data.Either.Validation (Validation (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (FromDhall)
import FileHandler (InputInfo, MoveError, NamingError)
import GHC.Generics (Generic)
import Parser (FileInfo)
import Sorting (SortingError)
import Prelude hiding (log)

-- | Level of logging equal to or above which to print.
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, FromDhall, Generic, Show)

-- | Where to output logging information.
data LogDestination
  = File FilePath
  | StdErr
  | StdOut
  deriving (Eq, FromDhall, Generic)

instance Show LogDestination where
  show (File p) = "Log to file: " <> show p
  show StdErr = "stderr"
  show StdOut = "stdout"

-- | Log files that were ignored due to ignore patterns.
logIgnored :: Map FilePath (InputInfo, Text) -> IO ()
logIgnored fs = unless (M.null fs) $ do
  log "Ignored files:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p (i, iPat) _ = do
      log $ "Ignoring file at: " <> T.pack (show p)
      debug $ "Input info: " <> T.pack (show i)
      debug $ "Ignored by: " <> T.pack (show iPat)

-- | Log files that were loaded and not ignored.
logLoaded :: Map FilePath InputInfo -> IO ()
logLoaded fs = unless (M.null fs) $ do
  log "Loaded files:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile path i _ = do
      log $ "Loaded file at: " <> T.pack (show path)
      debug $ "Input info: " <> T.pack (show i)

-- | Log files that failed parsing and will thus not be used.
logParsingFailures :: Map FilePath () -> IO ()
logParsingFailures fs = unless (M.null fs) $ do
  warn "The following files failed tag parsing:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p _ _ =
      warn $ "The file at " <> T.pack (show p) <> " failed parsing with the given input format."

-- | Log files that passed parsing.  (This is only useful for debugging.)
logParsingSuccesses :: Map FilePath (InputInfo, FileInfo) -> IO ()
logParsingSuccesses fs = unless (M.null fs) $ do
  debug "Parsed files:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p (_, i) _ =
      debug $ "Parsed file at " <> T.pack (show p) <> " to:\n" <> T.pack (show i)

-- | Log files that failed to sort.
logSortingFailures :: Map FilePath SortingError -> IO ()
logSortingFailures fs = unless (M.null fs) $ do
  warn "The following files failed sorting:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p e _ =
      warn $ "File at " <> T.pack (show p) <> " failed sorting with the following error:\n" <> T.pack (show e)

-- | Log files that passed sorting.
logSortingSuccesses :: Map FilePath (InputInfo, FileInfo, FilePath) -> IO ()
logSortingSuccesses fs = unless (M.null fs) $ do
  log "Sorted files:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p (_, _, f) _ =
      log $ "File at " <> T.pack (show p) <> " to be sorted to folder: " <> T.pack (show f)

-- | Log files that failed renaming.
logRenamingFailures :: Map FilePath NamingError -> IO ()
logRenamingFailures fs = unless (M.null fs) $ do
  warn "The following files failed renaming:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p e _ =
      warn $ "File at " <> T.pack (show p) <> " failed renaming with the following error:\n" <> T.pack (show e)

-- | Log files that passed renaming.
logRenamingSuccesses :: Map FilePath (FilePath, Text) -> IO ()
logRenamingSuccesses fs = unless (M.null fs) $ do
  log "Renamed files:"
  M.foldrWithKey logFile (return ()) fs
  flushLog
  where
    logFile p (_, n) _ =
      log $ "File at " <> T.pack (show p) <> " to be renamed to: " <> n

-- | Log files to be moved.
logPlannedMoves :: Map FilePath FilePath -> Map FilePath FilePath -> IO ()
logPlannedMoves firstMoves secondMoves = unless (M.null firstMoves) $ do
  log "Moves to be performed:"
  M.foldrWithKey logFile (return ()) firstMoves
  flushLog
  where
    logFile p outP _ =
      case outP `M.lookup` secondMoves of
        Just finalP ->
          warn $
            T.concat
              [ "File at ",
                T.pack (show p),
                " to be moved to temporary destination ",
                T.pack (show outP),
                " and then to final destination at: ",
                T.pack (show finalP)
              ]
        Nothing ->
          log $ "File at " <> T.pack (show p) <> " to be moved to: " <> T.pack (show outP)

-- | Log the actual results of moving files.
logMoves :: Validation [MoveError] () -> Validation [MoveError] () -> IO ()
logMoves firstResults secondResults = do
  case (firstResults, secondResults) of
    (Success (), Success ()) ->
      log "All files moved successfully!"
    (Success (), Failure es) -> do
      warn "Initial moves were successful, but errors were encountered on secondary moves.  Some files may have been left as temporary files:"
      mapM_ (warn . T.pack . show) es
    (Failure es, Success ()) -> do
      warn "Some moves failed!  The following errors were encountered:"
      mapM_ (warn . T.pack . show) es
    (Failure e1s, Failure e2s) -> do
      warn "Some initial moves failed!"
      mapM_ (warn . T.pack . show) e1s
      warn "Errors were also encountered during secondary moves.  Some files may have been left as temporary files:"
      mapM_ (warn . T.pack . show) e2s
  flushLog
