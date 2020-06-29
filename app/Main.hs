{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Config (AutotagicalConfig (..))
import Control.Logging
import Data.Either.Validation (Validation (..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall as D
import FileHandler
  ( determineMoves,
    loadFiles,
    moveFiles,
    parseFiles,
    renameFiles,
    sortFiles,
  )
import Logging
  ( LogDestination (..),
    LogLevel (..),
    logIgnored,
    logLoaded,
    logMoves,
    logParsingFailures,
    logParsingSuccesses,
    logPlannedMoves,
    logRenamingFailures,
    logRenamingSuccesses,
    logSortingFailures,
    logSortingSuccesses,
  )
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config] -> do
      c <- D.detailed (D.input D.auto (T.pack config)) :: IO AutotagicalConfig
      case logLevel c of
        Debug ->
          setLogLevel LevelDebug
        Info ->
          setLogLevel LevelInfo
        Warn ->
          setLogLevel LevelWarn
        Error ->
          setLogLevel LevelError
      let logger =
            case logDestination c of
              StdErr ->
                withStderrLogging
              StdOut ->
                withStdoutLogging
              File f ->
                withFileLogging f
      logger $ do
        debug "Loaded Autotagical config:"
        debug' . T.pack . show $ c
        -- Load all files
        (ignored, loaded) <-
          timedLog' "Loading files" $
            loadFiles (inputPatterns c) (ignorePatterns c) (fst <$> renaming c) (inputFolders c)
        logIgnored ignored
        logLoaded loaded
        -- Parse files
        (failedParsing, parsed) <-
          timedLog' "Parsing files" $ return $ parseFiles (inputFormat c) loaded
        logParsingFailures failedParsing
        logParsingSuccesses parsed
        -- Sort files
        (failedSorting, sorted) <-
          timedLog' "Sorting files" $ return $ sortFiles (sortingSchema c) parsed
        logSortingFailures failedSorting
        logSortingSuccesses sorted
        -- Rename files
        (failedRenaming, renamed) <-
          timedLog' "Renaming files" $ return $
            renameFiles (inputFormat c) (outputFormat c) (snd <$> renaming c) sorted
        logRenamingFailures failedRenaming
        logRenamingSuccesses renamed
        -- Determine moves
        (firstMove, secondMove) <-
          timedLog' "Determining file moves" $ return $ determineMoves renamed
        logPlannedMoves firstMove secondMove
        -- Actually move files
        moveResults <-
          timedLog' "Moving files" $
            moveFiles (clobberDestination c) (keepCopyInInputFolder c) (outputFolders c) firstMove
        secondMoveResults <-
          if M.null secondMove
            then return $ Success ()
            else-- Never keep copy of temporary files
              timedLog' "Performing second moves" $ moveFiles (clobberDestination c) False (outputFolders c) secondMove
        logMoves moveResults secondMoveResults
    _ -> putStrLn "Autotagical requires exactly one argument, a Dhall expression that is a valid Config."
