{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : FileHandler
-- Description : Load, filter, sort, rename, and move files
--
-- Loads files from disk, processes them, and moves them.
module FileHandler
  ( -- * Loading
    GlobPatterns (..),
    InputInfo (..),
    loadFiles,
    parseFiles,

    -- * Sorting
    sortFiles,

    -- * Naming
    NamingError (..),
    renameFiles,

    -- * Moving
    ClobberDestination (..),
    MoveError (..),
    determineMoves,
    moveFiles,

    -- * Internal For Testing
    UnnamedStatus (..),
    copyFileToPath,
    moveFile,
  )
where

import Control.Exception (IOException, try)
import Control.Logging
import Control.Monad (filterM, unless)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Either.Combinators (mapBoth)
import Data.Either.Validation (Validation (..), eitherToValidation)
import Data.Foldable (sequenceA_)
import Data.Hashable
import Data.List (find, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (Decoder (..), FromDhall)
import qualified Dhall as D
import GHC.Exts (IsList (..))
import Parser
  ( FileInfo (..),
    InputFormat (..),
    OutputFormat,
    fileParser,
    parseFileInfo,
  )
import Renaming
  ( OutputFormatError,
    RenamingError,
    RenamingSchema,
    nameBySchema,
    writeFileName,
  )
import SafeType (NonEmptyList, safeText)
import Sorting (SortingError, SortingSchema, sortBySchema)
import System.Directory
  ( copyFileWithMetadata,
    createDirectoryIfMissing,
    doesFileExist,
    doesPathExist,
    makeAbsolute,
    removeFile,
  )
import System.FilePath ((</>), makeRelative, takeDirectory)
import qualified System.FilePath.Glob as G
import System.FilePath.Glob (CompOptions (..), Pattern)

-- * Type aliases for type signature convenience.

-- | The (absolute) path to the starting location of an input file.
type InputPath = FilePath

-- | The path to the output location for a file, relative to output folders.
type OutputPath = FilePath

-- | A temporary path for a file that has to be moved in two steps, relative to
--   output folders.
type TemporaryPath = FilePath

-- | The folder a file is sorted to, relative to output folders.
type SortedFolder = FilePath

-- | The output name and extension of a file, without tags, used for
--   deduplication.
type FileNameExt = Text

-- | The full output name of a file with tags written.
type WriteFileName = Text

-- | Whether or not to overwrite files already in the destination.  Wrapped so
--   as to have a Dhall instance that's not user-friendly.
newtype ClobberDestination = ClobberDestination Bool deriving (Eq)

instance Show ClobberDestination where
  show (ClobberDestination b) = "Overwrite destination files: " <> show b

-- | Dhall instance that, instead of using a boolean, requires input of a
--   disclaimer, so no one accidentally overwrites important files.
instance FromDhall ClobberDestination where
  autoWith _ = validate $ D.maybe D.strictText
    where
      validate (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success Nothing -> Success $ ClobberDestination False
            Success (Just "I understand that data loss may occur; I want to overwrite destination files.") ->
              Success $ ClobberDestination True
            Success (Just t) ->
              D.extractError $ "clobberDestination was set to an invalid text value.  If you really want to overwrite destination files, please set the value to exactly: Some \"I understand that data loss may occur; I want to overwrite destination files.\"\nYou entered: " <> T.pack (show t)
            Failure f -> Failure f

-- | Info on a file as initially read in (pre-parsing), including information on
--   what patterns it matched and if it is unnamed.
data InputInfo
  = InputInfo
      { patternMatched :: Text,
        inputFolder :: FilePath,
        unnamedStatus :: UnnamedStatus
      }
  deriving (Eq)

instance Show InputInfo where
  show InputInfo {patternMatched, inputFolder, unnamedStatus} =
    unlines
      [ "Input information:",
        "  Folder: " <> inputFolder,
        "  Matched pattern: " <> show patternMatched,
        "  Unnamed: " <> show unnamedStatus
      ]

-- | Holds the matched unnamed pattern if unnamed.
data UnnamedStatus
  = Named
  | Unnamed {unnamedPattern :: Text}
  deriving (Eq)

instance Show UnnamedStatus where
  show Named = "No"
  show Unnamed {unnamedPattern} =
    "Yes (matched pattern: " <> show unnamedPattern <> " )"

-- | Glob patterns for input, ignore, and unnamed.  Holds the pattern it was
--   compiled from as well for logging reasons.
newtype GlobPatterns = GlobPatterns {patterns :: NonEmptyList (Text, Pattern)} deriving (Eq)

instance Show GlobPatterns where
  show GlobPatterns {patterns} =
    concat ["[", intercalate ", " . toList $ show . fst <$> patterns, "]"]

-- | Define a `FromDhall` instance for `GlobPatterns` that compiles them or
--   fails with a nice error.
instance FromDhall GlobPatterns where
  autoWith _ =
    compile $
      D.record
        ( (,)
            <$> D.field "patterns" (D.list D.string)
            <*> D.field "errorRecovery" D.bool
        )
    where
      compile (Decoder input expect) = Decoder out expect
        where
          out e = case input e of
            Success (ps, errorRecovery) ->
              case traverse
                ( \p ->
                    eitherToValidation . mapBoth (\err -> [(p, err)]) (T.pack p,)
                      . G.tryCompileWith (G.compDefault {errorRecovery})
                      $ p
                )
                ps of
                Success [] -> D.extractError "Must have at least one glob pattern, but list was empty."
                Success res -> Success . GlobPatterns . fromList $ res
                Failure es ->
                  D.extractError . T.concat
                    . fmap
                      ( \(p, err) ->
                          T.concat
                            [ "Encountered invalid glob pattern: ",
                              T.pack (show p),
                              "\n",
                              T.pack err,
                              "\n"
                            ]
                      )
                    $ es
            Failure f -> Failure f

-- | Partition a list based on an Either
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith p = foldr select ([], [])
  where
    select x ~(ls, rs) = case p x of
      Left l -> (l : ls, rs)
      Right r -> (ls, r : rs)

-- | Match files against a list of patterns, returning the matched pattern.
matchByPattern :: FilePath -> Maybe GlobPatterns -> FilePath -> Either FilePath (FilePath, Text)
matchByPattern _ Nothing file = Left file
matchByPattern dir (Just (GlobPatterns globPatterns)) file =
  go (toList globPatterns)
  where
    f = makeRelative dir file
    go [] = Left file
    go ((t, p) : ps)
      | G.match p f = Right (file, t)
      | otherwise = go ps

-- | Given input and ignore patterns and a list of input directories, build
--   maps of ignored and input files.
loadFiles ::
  GlobPatterns ->
  Maybe GlobPatterns ->
  Maybe GlobPatterns ->
  NonEmptyList FilePath ->
  IO (Map InputPath (InputInfo, Text), Map InputPath InputInfo)
loadFiles (GlobPatterns inPs) ignorePs unnamedPs inDirs =
  ( \(ign, inp) ->
      let ignored = M.fromList $ concat ign
       in ( ignored,
            -- Remove files that were both ignored and input
            M.fromListWith keepUnnamed (concat inp) `M.difference` ignored
          )
  )
    . unzip
    <$> sequence
      ( ( \p dir -> do
            files <- G.globDir1 (snd p) dir
            nonDirectories <- filterM doesFileExist files
            let (keepFiles, ignoreFiles) = partitionWith (matchByPattern dir ignorePs) nonDirectories
            processedKeep <-
              mapM (\(f, i) -> (,i) <$> makeAbsolute f) $
                ( \f ->
                    ( f,
                      InputInfo
                        (fst p)
                        dir
                        ( case matchByPattern dir unnamedPs f of
                            Left _ ->
                              Named
                            Right (_, uP) ->
                              Unnamed uP
                        )
                    )
                )
                  <$> keepFiles
            processedIgnored <-
              mapM (\(f, i) -> (,i) <$> makeAbsolute f) $
                (\(f, ignP) -> (f, (InputInfo (fst p) dir Named, ignP)))
                  <$> ignoreFiles
            return (processedIgnored, processedKeep)
        )
          <$> toList inPs
          <*> toList inDirs
      )
  where
    keepUnnamed :: InputInfo -> InputInfo -> InputInfo
    keepUnnamed a b
      | unnamedStatus b == Named = a
      | otherwise = b

-- | Convert a `Maybe` to an `Either`, given an error value.
maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e Nothing = Left e
maybeToRight _ (Just a) = Right a

-- | Parse filenames into `FileInfo`, accumulating info on files that error out.
parseFiles ::
  InputFormat ->
  Map InputPath InputInfo ->
  (Map InputPath (), Map InputPath (InputInfo, FileInfo))
parseFiles f@InputFormat {tagOrder} =
  M.mapEitherWithKey
    (\k v -> maybeToRight () $ (v,) <$> parseFileInfo tagOrder p k)
  where
    p = fileParser f

-- | Sort files according to a schema
sortFiles ::
  SortingSchema ->
  Map InputPath (InputInfo, FileInfo) ->
  (Map InputPath SortingError, Map InputPath (InputInfo, FileInfo, SortedFolder))
sortFiles s =
  M.mapEitherWithKey (\_ (i, f) -> (i,f,) <$> sortBySchema s f)

-- | Specify the way in which a file failed to be named.
--
--   [`CollisionBetweenDifferentRenamed`] Some number of files were assigned
--     the same initial name and extensions and then successfully renamed to
--     non-overlapping duplicate names.  However, one of more of these duplicate
--     names collided with an already deduplicated file that was not one of the
--     original duplicates.
--   [`CollisionBetweenRenamedAndInitial`] Some number of files were assigned
--     the same initial name and extension and then successfully renamed to
--     non-overlapping duplicate names.  However, one or more of these
--     duplicate names collided with an original file that was not one of the
--     original duplicates.
--   [`CollisionBetweenRenamedDuplicates`] Some number of files were assigned
--     the same initial name and extension, but subsequent attempts to produce
--     non-colliding duplicate names resulted in another collision.
--   ['CollisionBetweenUnnamedAndNamed'] A renamed file collided with a file
--     that wasn't to be renamed.  Since a file not to be renamed can't be
--     deduplicated, none can be moved.
--   [`DuplicateFileFailedRename`] Some number of files were assigned the same
--     initial name and extension, but one or more of them failed subsequent
--     renaming.
--   [`DuplicateFileWithoutRenamingSchema`] Some number of files were assigned
--     the same initial name and extension, but no `RenamingSchema` was
--     provided.
--   [`OutputFormatErrors`] A file failed renaming due to one or more
--     `OutputFormatError`s.  This usually means that there was an invalid
--     tag/tagged-value for the format, e.g. a tagged-value with an
--     `OutputFormat` that forbids them.
--   [`FileHandler.RenamingError`] A file failed renaming due to one or more
--    `Renaming.RenamingError`s.  This usually means either no
--     `Renaming.RenamingRule` was found or something was wrong with the name
--     template that was found.
data NamingError
  = CollisionBetweenDifferentRenamed
      { fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt,
        outNameExt :: FileNameExt,
        collidingNameExt :: FileNameExt
      }
  | CollisionBetweenRenamedAndInitial
      { fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt,
        outNameExt :: FileNameExt,
        collidingNameExt :: FileNameExt
      }
  | CollisionBetweenRenamedDuplicates
      { fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt,
        outNameExt :: FileNameExt
      }
  | CollisionBetweenUnnamedAndNamed
      { namedFileInfo :: FileInfo,
        fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt
      }
  | DuplicateFileFailedRename
      { failedErr :: Maybe NamingError,
        fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt
      }
  | DuplicateFileWithoutRenamingSchema
      { fileInfo :: FileInfo,
        sortedFolder :: SortedFolder,
        initNameExt :: FileNameExt
      }
  | OutputFormatErrors {fileInfo :: FileInfo, errors :: [OutputFormatError]}
  | RenamingError RenamingError
  deriving (Eq)

instance Show NamingError where
  show
    CollisionBetweenDifferentRenamed
      { fileInfo,
        sortedFolder,
        initNameExt,
        outNameExt,
        collidingNameExt
      } =
      unlines
        [ "Collision between different deduplicated files!",
          "Some number of files were assigned the same initial name and extension and then successfully renamed to non-overlapping duplicate names.  However, one of more of these names collided with an already deduplicated file.",
          "Output directory: " <> show sortedFolder,
          "Failed file:",
          unlines . fmap ("  " ++) . lines . show $ fileInfo,
          "Initial name + extension: " <> show initNameExt,
          "Name + extension after deduplication: " <> show outNameExt,
          "Name + extension collided with: " <> show collidingNameExt
        ]
  show
    CollisionBetweenRenamedAndInitial
      { fileInfo,
        sortedFolder,
        initNameExt,
        outNameExt,
        collidingNameExt
      } =
      unlines
        [ "Collision between deduplicated files and initial named files!",
          "Some number of files were assigned the same initial name and extension and then successfully renamed to non-overlapping duplicate names.  However, one or more of these duplicate names collided with an initial file name.",
          "Output directory: " <> show sortedFolder,
          "Failed file:",
          unlines . fmap ("  " ++) . lines . show $ fileInfo,
          "Initial name + extension: " <> show initNameExt,
          "Name + extension after deduplication: " <> show outNameExt,
          "Name + extension collided with: " <> show collidingNameExt
        ]
  show
    CollisionBetweenRenamedDuplicates
      { fileInfo,
        sortedFolder,
        initNameExt,
        outNameExt
      } =
      unlines
        [ "Collision between deduplicated file names!",
          "Some number of files were assigned the same initial name and extension, but subsequent attempts to produce non-colliding duplicate names resulted in another collision.  This usually means the relevant name template lacked a duplicate number component.",
          "Output directory: " <> show sortedFolder,
          "Failed file:",
          unlines . fmap ("  " ++) . lines . show $ fileInfo,
          "Initial name + extension: " <> show initNameExt,
          "Name + extension after deduplication: " <> show outNameExt
        ]
  show
    CollisionBetweenUnnamedAndNamed
      { namedFileInfo,
        fileInfo,
        sortedFolder,
        initNameExt
      } =
      unlines
        [ "Collision between renamed files and an un-renamed file!",
          "Some number of files were assigned the same initial name and extension, but one or more of them was not renamed!  Since an already-named file can't have a deduplicated name generated, they all had to be failed.",
          "Output directory: " <> show sortedFolder,
          "Failed file:",
          unlines . fmap ("  " ++) . lines . show $ fileInfo,
          "Named file collided with:",
          unlines . fmap ("  " ++) . lines . show $ namedFileInfo,
          "Initial name + extension: " <> show initNameExt
        ]
  show
    DuplicateFileFailedRename
      { failedErr,
        fileInfo,
        sortedFolder,
        initNameExt
      } =
      unlines $
        [ "A duplicate file failed the renaming process!",
          "Some number of files were assigned the same initial name and extension, but one or more of them failed in the renaming process!",
          "Output directory: " <> show sortedFolder,
          "Failed file:",
          unlines . fmap ("  " ++) . lines . show $ fileInfo,
          "Initial name + extension: " <> show initNameExt
        ]
          ++ case failedErr of
            Nothing -> []
            Just e ->
              [ "This is the file that failed renaming, with the error: ",
                unlines . fmap ("  " ++) . lines . show $ e
              ]
  show DuplicateFileWithoutRenamingSchema {fileInfo, sortedFolder, initNameExt} =
    unlines
      [ "Duplicate files without any renaming schema!",
        "Some number of files were assigned the same initial name and extension, but they could not be deduplicated because no schema for renaming them was provided!  This means you had two files with the same name that got sorted to the same folder.",
        "Output directory: " <> show sortedFolder,
        "Failed file:",
        unlines . fmap ("  " ++) . lines . show $ fileInfo,
        "Initial name + extension: " <> show initNameExt
      ]
  show OutputFormatErrors {fileInfo, errors} =
    unlines $
      [ "A file failed because of incompatibility with the output format.",
        "Failed file:",
        unlines . fmap ("  " ++) . lines . show $ fileInfo,
        "The following errors occurred:"
      ]
        ++ fmap (unlines . fmap ("  " ++) . lines . show) errors
  show (RenamingError e) =
    unlines
      [ "A file failed the renaming process with the following error:",
        unlines . fmap ("  " ++) . lines . show $ e
      ]

-- | Given sorted files, try initial naming of them, returning errored files,
--   and a map of successful ones grouped by output folder, name, and extension.
groupByInitialName ::
  InputFormat ->
  Maybe OutputFormat ->
  Maybe RenamingSchema ->
  Map InputPath (InputInfo, FileInfo, SortedFolder) ->
  ( Map InputPath NamingError,
    Map (SortedFolder, FileNameExt)
      [(InputPath, InputInfo, FileInfo, WriteFileName)]
  )
groupByInitialName inFormat outFormat schema =
  M.foldrWithKey
    ( \inPath (inInfo, fInfo, sortDir) (errAcc, groupAcc) ->
        let outputInfo =
              ( case (schema, unnamedStatus inInfo) of
                  (_, Named) ->
                    Right Nothing
                  (Just s, Unnamed _) ->
                    first RenamingError $ Just <$> nameBySchema s Nothing fInfo
                  (Nothing, Unnamed _) ->
                    error "Encountered an unnamed file without a renaming schema!  This absolutely should not be possible, so please report it as a bug."
              )
                >>= ( \name ->
                        (,fromMaybe (safeText $ originalName fInfo) name
                          <> maybe "" safeText (extension fInfo))
                          <$> ( first (OutputFormatErrors fInfo)
                                  . writeFileName inFormat outFormat fInfo
                                  $ name
                              )
                    )
         in case outputInfo of
              Left e -> (M.insert inPath e errAcc, groupAcc)
              Right (writeName, nameExt) ->
                ( errAcc,
                  M.insertWith
                    (++)
                    (sortDir, nameExt)
                    [(inPath, inInfo, fInfo, writeName)]
                    groupAcc
                )
    )
    (M.empty, M.empty)

-- | Given a `RenamingSchema` and a map of parsed and sorted files, determine
--   their final file names, returning files that error out and finalized files.
renameFiles ::
  InputFormat ->
  Maybe OutputFormat ->
  Maybe RenamingSchema ->
  Map InputPath (InputInfo, FileInfo, SortedFolder) ->
  ( Map InputPath NamingError,
    Map InputPath (SortedFolder, WriteFileName)
  )
renameFiles inFormat outFormat schema fs =
  ( M.union erroredFiles erroredWhileDeduping,
    -- Switch info around now that we're done deduping
    M.foldrWithKey
      ( \(sortDir, _) (inPath, _, _, outName) ->
          M.insert inPath (sortDir, outName)
      )
      M.empty
      dedupedFiles
  )
  where
    (erroredFiles, groupedByDirNameExt) =
      groupByInitialName inFormat outFormat schema fs
    (erroredWhileDeduping, dedupedFiles) =
      M.foldrWithKey dedupeFiles (M.empty, M.empty) groupedByDirNameExt
    dedupeFiles ::
      (SortedFolder, FileNameExt) ->
      [(InputPath, InputInfo, FileInfo, WriteFileName)] ->
      ( Map InputPath NamingError,
        Map (SortedFolder, FileNameExt)
          (InputPath, InputInfo, FileInfo, WriteFileName)
      ) ->
      ( Map InputPath NamingError,
        Map (SortedFolder, FileNameExt)
          (InputPath, InputInfo, FileInfo, WriteFileName)
      )
    -- If there is only one file with a given output name/path, then no need for
    -- further renaming
    dedupeFiles p [d] (errAcc, validAcc) =
      (errAcc, M.insert p d validAcc)
    -- If any of the collisions are not a file that's to be renamed, fail them
    -- all, since we can't deduplicate name a named file.
    dedupeFiles (sortDir, initNameExt) dupes (errAcc, validAcc)
      | Just (_, _, namedFInfo, _) <-
          find (\(_, inInfo, _, _) -> unnamedStatus inInfo == Named) dupes =
        ( foldr
            ( \(inPath, _, fInfo, _) ->
                M.insert
                  inPath
                  (CollisionBetweenUnnamedAndNamed namedFInfo fInfo sortDir initNameExt)
            )
            errAcc
            dupes,
          validAcc
        )
    -- If there is more than one file mapped to a given output name/path and
    -- all are unnamed, need to deduplicate them.
    dedupeFiles (sortDir, initNameExt) dupes (errAcc, validAcc)
      -- If any file failed the duplicate renaming, fail all of them.
      | any (\(_, _, _, n) -> isLeft n) renamed =
        ( foldr
            ( \(inPath, _, info, newName) ->
                M.insert
                  inPath
                  ( case newName of
                      Left err ->
                        DuplicateFileFailedRename (Just err) info sortDir initNameExt
                      Right _ ->
                        DuplicateFileFailedRename Nothing info sortDir initNameExt
                  )
            )
            errAcc
            renamed,
          validAcc
        )
      -- If duplicate renaming produced a collision between the renamed files,
      -- fail them all.
      | S.size
          ( S.fromList
              ((\(_, _, _, n) -> either (const "") snd n) <$> renamed)
          )
          /= length renamed =
        ( foldr
            ( \(inPath, _, fInfo, n) ->
                M.insert
                  inPath
                  ( CollisionBetweenRenamedDuplicates
                      fInfo
                      sortDir
                      initNameExt
                      (either (const "") snd n)
                  )
            )
            errAcc
            renamed,
          validAcc
        )
      -- If duplicate renaming produced a collision with any initial file, fail
      -- them all (normally named file takes precedence).
      | Just (_, _, _, Right (_, collideNameExt)) <-
          find
            ( \(_, _, _, newName) ->
                M.member (sortDir, either (const "") snd newName) groupedByDirNameExt
            )
            renamed =
        ( foldr
            ( \(inPath, _, fInfo, n) ->
                M.insert
                  inPath
                  ( CollisionBetweenRenamedAndInitial
                      fInfo
                      sortDir
                      initNameExt
                      (either (const "") snd n)
                      collideNameExt
                  )
            )
            errAcc
            renamed,
          validAcc
        )
      -- If duplicate renaming produced a collision with something already
      -- deduplicated, fail them all.  (Can't go back and fail the previous
      -- ones, since we don't know whether they were originally duplicates.)
      | Just (_, _, _, Right (_, collideNameExt)) <-
          find
            ( \(_, _, _, newName) ->
                M.member (sortDir, either (const "") snd newName) validAcc
            )
            renamed =
        ( foldr
            ( \(inPath, _, fInfo, n) ->
                M.insert
                  inPath
                  ( CollisionBetweenDifferentRenamed
                      fInfo
                      sortDir
                      initNameExt
                      (either (const "") snd n)
                      collideNameExt
                  )
            )
            errAcc
            renamed,
          validAcc
        )
      -- If none of the above occurred, files were de-duplicated successfully.
      | otherwise =
        ( errAcc,
          foldr
            ( \(inPath, inInfo, fInfo, newName) ->
                M.insert
                  (sortDir, either (const "") snd newName)
                  (inPath, inInfo, fInfo, either (const "") fst newName)
            )
            validAcc
            renamed
        )
      where
        renamed ::
          [ ( InputPath,
              InputInfo,
              FileInfo,
              Either NamingError (WriteFileName, FileNameExt)
            )
          ]
        renamed =
          ( \(dupeN, (inPath, inInfo, fInfo, _)) ->
              ( inPath,
                inInfo,
                fInfo,
                case schema of
                  Nothing ->
                    Left $ DuplicateFileWithoutRenamingSchema fInfo sortDir initNameExt
                  Just s ->
                    first RenamingError (Just <$> nameBySchema s (Just dupeN) fInfo)
                      >>= ( \name ->
                              (,fromMaybe (safeText $ originalName fInfo) name
                                <> maybe "" safeText (extension fInfo))
                                <$> ( first (OutputFormatErrors fInfo)
                                        . writeFileName inFormat outFormat fInfo
                                        $ name
                                    )
                          )
              )
          )
            <$> zip [1 ..] dupes

-- | Filter out files that would simply be moved to their current location.
--   Additionally, check for files whose destination is equivalent to the
--   original location of another file.  In these cases, move them to a
--   temporary path and then move them in a second round.
determineMoves ::
  Map InputPath (SortedFolder, WriteFileName) ->
  ( Map InputPath OutputPath,
    Map TemporaryPath OutputPath
  )
determineMoves fs =
  M.foldrWithKey
    ( \inPath (sortedFolder, outPath) (firstMoves, secondMoves) ->
        if M.member outPath fs
          then
            let tempPath = sortedFolder </> show (hash outPath)
             in ( M.insert inPath tempPath firstMoves,
                  M.insert tempPath outPath secondMoves
                )
          else (M.insert inPath outPath firstMoves, secondMoves)
    )
    (M.empty, M.empty)
    filtered
  where
    -- No need to move things in place.
    filtered =
      M.mapMaybeWithKey
        ( \inPath (sortedFolder, outName) ->
            let outPath = sortedFolder </> T.unpack outName
             in if outPath == inPath
                  then Nothing
                  else Just (sortedFolder, outPath)
        )
        fs

-- | Specify the way in which moving a file failed.
--
--   [`DestinationFileExists`] There was already a file in the output location
--     (not one that Autotagical moved, since that would have been caught
--     earlier).
--   [`DirectoryInDestination`] There was already a directory in the output
--     location.
--   [`FileCopyException`] An exception occurred while attempting to copy a file
--     to the output location.  This usually is a permission error or something
--     along those lines.
--   [`FileRemovalException`] An exception occurred while attempting to remove
--     an input file that was copied successfully.  This usually is a permission
--     error or something along those lines.
data MoveError
  = DestinationFileExists {inputPath :: FilePath, outputPath :: FilePath}
  | DirectoryInDestination {inputPath :: FilePath, outputPath :: FilePath}
  | FileCopyException
      { inputPath :: FilePath,
        outputPath :: FilePath,
        exception :: Text
      }
  | FileRemovalException {inputPath :: FilePath, exception :: Text}
  deriving (Eq)

instance Show MoveError where
  show DestinationFileExists {inputPath, outputPath} =
    unlines
      [ "The file at:",
        inputPath,
        "could not be moved to:",
        outputPath,
        "because a file already exists at that location."
      ]
  show DirectoryInDestination {inputPath, outputPath} =
    unlines
      [ "The file at:",
        inputPath,
        "could not be moved to:",
        outputPath,
        "because a directory already exists at that location."
      ]
  show FileCopyException {inputPath, outputPath, exception} =
    unlines
      [ "The file at:",
        inputPath,
        "could not be moved to:",
        outputPath,
        "because the following error occurred:",
        T.unpack exception
      ]
  show FileRemovalException {inputPath, exception} =
    unlines
      [ "The file at:",
        inputPath,
        "was successfully copied to its destination, but the original could not be removed because the following error occurred:",
        T.unpack exception
      ]

-- | Given whether to clobber destination, whether to keep a copy of the input
--   files, a list of output folders, and a list of moves to perform, actually
--   perform said file moves.
moveFiles ::
  Bool ->
  ClobberDestination ->
  Bool ->
  NonEmptyList FilePath ->
  Map InputPath OutputPath ->
  IO (Validation [MoveError] ())
moveFiles dryRun clobber keepCopy outputDirs fs =
  sequenceA_ <$> mapM (moveFile dryRun clobber keepCopy outputDirs) (M.assocs fs)

-- | Given whether to clobber destination, whether to keep a copy of the input
--   file, a list of output folders, and a move to perform, actually perform
--   said file move, with appropriate logging.
moveFile ::
  Bool ->
  ClobberDestination ->
  Bool ->
  NonEmptyList FilePath ->
  (InputPath, OutputPath) ->
  IO (Validation [MoveError] ())
moveFile dryRun clobber keepCopy outputDirs (inPath, destPath) = do
  moves <- mapM (\d -> copyFileToPath dryRun clobber inPath (d </> destPath)) (toList outputDirs)
  case sequenceA_ moves of
    Success () ->
      if keepCopy
        then do
          debug $
            "File at "
              <> T.pack inPath
              <> " moved successfully.  Keeping a copy in place."
          return $ Success ()
        else do
          debug $
            "File at "
              <> T.pack inPath
              <> " moved successfully.  Removing original."
          eitherToValidation
            . first ((: []) . FileRemovalException inPath . T.pack . show)
              <$> ( try $ unless dryRun $
                      removeFile inPath ::
                      IO (Either IOException ())
                  )
    Failure es -> do
      warn $
        "Due to copying errors, the original file at "
          <> T.pack inPath
          <> " was not removed."
      return $ Failure es

-- | Given whether to clobber destination, an output path, and an input path,
--   copy a file or fail with a descriptive error.
copyFileToPath ::
  Bool ->
  ClobberDestination ->
  FilePath ->
  FilePath ->
  IO (Validation [MoveError] ())
copyFileToPath dryRun clobber inPath outPath = do
  pathExists <- doesPathExist outPath
  if not pathExists
    then do
      unless dryRun $ createDirectoryIfMissing True (takeDirectory outPath)
      debug $ "Copying file from " <> T.pack inPath <> " to " <> T.pack outPath
      eitherToValidation
        . first ((: []) . FileCopyException inPath outPath . T.pack . show)
          <$> ( try $ unless dryRun $
                  copyFileWithMetadata inPath outPath ::
                  IO (Either IOException ())
              )
    else do
      fileExists <- doesFileExist outPath
      if fileExists
        then case clobber of
          ClobberDestination True -> do
            warn $
              "Overwriting existing file at "
                <> T.pack outPath
                <> " with file at "
                <> T.pack inPath
            eitherToValidation
              . first ((: []) . FileCopyException inPath outPath . T.pack . show)
                <$> ( try $ unless dryRun $
                        copyFileWithMetadata inPath outPath ::
                        IO (Either IOException ())
                    )
          ClobberDestination False ->
            return $ Failure [DestinationFileExists inPath outPath]
        else return $ Failure [DirectoryInDestination inPath outPath]
