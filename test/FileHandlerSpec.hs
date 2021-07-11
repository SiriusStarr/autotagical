{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module FileHandlerSpec
  ( spec,
  )
where

import Control.Logging
import qualified Data.Either.Validation as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import FileHandler
  ( ClobberDestination (..),
    GlobPatterns (..),
    InputInfo (..),
    MoveError (..),
    NamingError (..),
    UnnamedStatus (..),
    copyFileToPath,
    determineMoves,
    loadFiles,
    moveFile,
    moveFiles,
    parseFiles,
    renameFiles,
    sortFiles,
  )
import NameTemplate
  ( FileName (..),
    FileNameComponent (..),
    FolderName (..),
    FolderNameComponent (..),
    NameTemplateError (..),
  )
import Parser
  ( FileInfo (..),
    InputFormat (..),
    Order (..),
    Separator (..),
    SeparatorRequirement (..),
  )
import Predicate (Predicate (..))
import Renaming (RenamingError (..), RenamingRule (..), RenamingSchema (..))
import Sorting (Folder (..), SortingError (..), SortingSchema (..))
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    makeAbsolute,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Tag (Tag (..), TagValue (..), Tags (..))
import Test.Hspec
import Utility (importFails, importSucceeds, makePatterns)

-- | Given a temp dir and a list of files/patterns, create a list of files to
--   expect given that parent folder.
expectFiles ::
  FilePath ->
  [(FilePath, Text, UnnamedStatus)] ->
  IO (Map FilePath InputInfo)
expectFiles dir =
  fmap M.fromList
    . mapM
      ( \(f, p, named) ->
          (,InputInfo p dir named)
            <$> makeAbsolute (dir </> f)
      )

-- | Given a temp dir and a list of files/ patterns, create a list of files to
--   expect ignored given that parent folder.
expectIgnoredFiles ::
  FilePath ->
  [(FilePath, Text, UnnamedStatus, Text)] ->
  IO (Map FilePath (InputInfo, Text))
expectIgnoredFiles dir =
  fmap M.fromList
    . mapM
      ( \(f, p, named, iP) ->
          (,(InputInfo p dir named, iP))
            <$> makeAbsolute (dir </> f)
      )

-- | Given a temp dir, set up the files we'll be testing loading relative to.
prepareTestFiles :: FilePath -> IO ()
prepareTestFiles dir = do
  writeFile (dir </> "ignore this") "test"
  writeFile (dir </> "ignore this") "test"
  writeFile (dir </> "anotherFile.txt") "test"
  writeFile (dir </> "unnamed 1.txt") "test"
  writeFile (dir </> "unnamed 2.txt") "test"
  createDirectory (dir </> "subdirectory")
  writeFile (dir </> "subdirectory" </> "a file") "test"
  writeFile (dir </> "subdirectory" </> "also ignore this") "test"
  writeFile (dir </> "subdirectory" </> "unnamed 9.txt") "test"

-- | A placeholder `InputInfo` for when it doesn't matter
simpleInputInfo :: InputInfo
simpleInputInfo = InputInfo "" "" $ Unnamed ""

spec :: Spec
spec = do
  context "FromDhall instances" $ do
    describe "ClobberDestination" $ do
      let d = autoWith defaultInputNormalizer :: Decoder ClobberDestination
      it "imports successfully when False" $
        importSucceeds d (ClobberDestination False) "clobberDestination-false.dhall"
      it "imports successfully when True" $
        importSucceeds d (ClobberDestination True) "clobberDestination-true.dhall"
      it "fails on other text" $
        importFails d "clobberDestination-wrongText.dhall"
      it "fails on empty text" $
        importFails d "clobberDestination-empty.dhall"
    describe "GlobPatterns" $ do
      let d = autoWith defaultInputNormalizer :: Decoder GlobPatterns
      it "imports successfully with valid patterns" $
        importSucceeds d (makePatterns False ["*.jpg", "*/*.png", "literal"]) "globPatterns-noError.dhall"
      it "imports successfully with invalid patterns with error recovery" $
        importSucceeds d (makePatterns True ["*.jpg", "*[/*.png", "literal"]) "globPatterns-withError.dhall"
      it "fails on invalid patters without error recovery" $
        importFails d "globPatterns-withErrorNoRecovery.dhall"
      it "fails on an empty list" $
        importFails d "globPatterns-empty.dhall"
  parallel $ do
    describe "parseFiles" $ do
      it "parses valid files" $ do
        let inputFiles =
              M.fromList
                [ ("file tag1,tag2 ", simpleInputInfo),
                  ("anotherFile tag,tag3 .ext", simpleInputInfo),
                  ("filefile tag ", simpleInputInfo)
                ]
        let inputFormat = InputFormat NameTagsExtension Whitespace Forbidden (Keyword ",") Nothing Forbidden Whitespace
        parseFiles inputFormat inputFiles
          `shouldBe` ( M.empty,
                       M.fromList
                         [ ( "file tag1,tag2 ",
                             (simpleInputInfo, FileInfo (Tags $ M.fromList [("tag1", Present), ("tag2", Present)]) " tag1,tag2 " "file" Nothing)
                           ),
                           ( "anotherFile tag,tag3 .ext",
                             (simpleInputInfo, FileInfo (Tags $ M.fromList [("tag", Present), ("tag3", Present)]) " tag,tag3 " "anotherFile" (Just ".ext"))
                           ),
                           ( "filefile tag ",
                             (simpleInputInfo, FileInfo (Tags $ M.fromList [("tag", Present)]) " tag " "filefile" Nothing)
                           )
                         ]
                     )
      it "invalidates files that can't be parsed" $ do
        let inputFiles =
              M.fromList
                [ ("file tag1,tag2 ", simpleInputInfo),
                  ("anotherFile tag,tag3 .ext", simpleInputInfo),
                  ("filefiletag ", simpleInputInfo)
                ]
        let inputFormat = InputFormat NameTagsExtension Whitespace Forbidden (Keyword ",") Nothing Forbidden Whitespace
        parseFiles inputFormat inputFiles
          `shouldBe` ( M.singleton "filefiletag " (),
                       M.fromList
                         [ ( "file tag1,tag2 ",
                             (simpleInputInfo, FileInfo (Tags $ M.fromList [("tag1", Present), ("tag2", Present)]) " tag1,tag2 " "file" Nothing)
                           ),
                           ( "anotherFile tag,tag3 .ext",
                             (simpleInputInfo, FileInfo (Tags $ M.fromList [("tag", Present), ("tag3", Present)]) " tag,tag3 " "anotherFile" (Just ".ext"))
                           )
                         ]
                     )
    describe "sortFiles" $ do
      it "sorts valid files" $ do
        let schema =
              SortingSchema
                [ Folder (FolderName [FolderTextLiteral "1"] Nothing) (HasTag "tag1" Nothing) [],
                  Folder (FolderName [FolderTextLiteral "2"] Nothing) (HasTag "tag2" Nothing) [Folder (FolderName [FolderTextLiteral "3"] Nothing) (HasTag "tag3" Nothing) []]
                ]
        let f1 = FileInfo (Tags $ M.fromList [("tag1", Present), ("tag2", Present)]) " tag1,tag2 " "file" Nothing
        let f2 = FileInfo (Tags $ M.fromList [("tag2", Present), ("tag3", Present)]) " tag2,tag3 " "anotherFile" (Just ".ext")
        let f3 = FileInfo (Tags $ M.fromList [("tag2", Present)]) " tag2 " "filefile" Nothing
        let inputFiles =
              M.fromList
                [ ("file tag1,tag2 ", (simpleInputInfo, f1)),
                  ("anotherFile tag2,tag3 .ext", (simpleInputInfo, f2)),
                  ("filefile tag2 ", (simpleInputInfo, f3))
                ]
        sortFiles schema inputFiles
          `shouldBe` ( M.empty,
                       M.fromList
                         [ ( "file tag1,tag2 ",
                             (simpleInputInfo, f1, "1")
                           ),
                           ( "anotherFile tag2,tag3 .ext",
                             (simpleInputInfo, f2, "2" </> "3")
                           ),
                           ( "filefile tag2 ",
                             (simpleInputInfo, f3, "2")
                           )
                         ]
                     )
      it "fails files when appropriate" $ do
        let schema =
              [ Folder (FolderName [FolderTextLiteral "1"] Nothing) (HasTag "tag1" Nothing) [],
                Folder (FolderName [FolderTextLiteral "3"] Nothing) (HasTag "tag3" Nothing) [Folder (FolderName [FolderTextLiteral "2"] Nothing) (HasTag "tag2" Nothing) []]
              ]
        let f1 = FileInfo (Tags $ M.fromList [("tag1", Present), ("tag2", Present)]) " tag1,tag2 " "file" Nothing
        let f2 = FileInfo (Tags $ M.fromList [("tag2", Present), ("tag3", Present)]) " tag2,tag3 " "anotherFile" (Just ".ext")
        let f3 = FileInfo (Tags $ M.fromList [("tag2", Present)]) " tag2 " "filefile" Nothing
        let inputFiles =
              M.fromList
                [ ("file tag1,tag2 ", (simpleInputInfo, f1)),
                  ("anotherFile tag2,tag3 .ext", (simpleInputInfo, f2)),
                  ("filefile tag2 ", (simpleInputInfo, f3))
                ]
        sortFiles (SortingSchema schema) inputFiles
          `shouldBe` ( M.singleton "filefile tag2 " $
                         NoSortingMatch f3,
                       M.fromList
                         [ ( "file tag1,tag2 ",
                             (simpleInputInfo, f1, "1")
                           ),
                           ( "anotherFile tag2,tag3 .ext",
                             (simpleInputInfo, f2, "3" </> "2")
                           )
                         ]
                     )
    describe "loadFiles" $ do
      it "loads files" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "*", Named),
                    ("anotherFile.txt", "*", Named),
                    ("unnamed 1.txt", "*", Named),
                    ("unnamed 2.txt", "*", Named)
                  ]
              loadFiles (makePatterns False ["*"]) Nothing Nothing [dir]
                `shouldReturn` (M.empty, expected)
          )
      it "loads files recursively" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "**/*", Named),
                    ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Named),
                    ("unnamed 2.txt", "**/*", Named),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "also ignore this", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Named)
                  ]
              loadFiles (makePatterns False ["**/*"]) Nothing Nothing [dir]
                `shouldReturn` (M.empty, expected)
          )
      it "records the pattern matched with" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("unnamed 1.txt", "*1*", Named),
                    ("unnamed 2.txt", "*2*", Named)
                  ]
              loadFiles (makePatterns False ["*1*", "*2*"]) Nothing Nothing [dir]
                `shouldReturn` (M.empty, expected)
          )
      it "obeys ignore patterns (relative to input dir" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              ignored <-
                expectIgnoredFiles
                  dir
                  [("ignore this", "**/*", Named, "*ignore*")]
              expected <-
                expectFiles
                  dir
                  [ ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Named),
                    ("unnamed 2.txt", "**/*", Named),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Named),
                    ("subdirectory" </> "also ignore this", "**/*", Named)
                  ]
              loadFiles
                (makePatterns False ["**/*"])
                (Just $ makePatterns False ["*ignore*"])
                Nothing
                [dir]
                `shouldReturn` (ignored, expected)
          )
      it "records the ignore pattern matched" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              ignored <-
                expectIgnoredFiles
                  dir
                  [ ("ignore this", "**/*", Named, "*ignore*"),
                    ("subdirectory" </> "also ignore this", "**/*", Named, "**/*ignore*")
                  ]
              expected <-
                expectFiles
                  dir
                  [ ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Named),
                    ("unnamed 2.txt", "**/*", Named),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Named)
                  ]
              loadFiles
                (makePatterns False ["**/*"])
                (Just $ makePatterns False ["*ignore*", "**/*ignore*"])
                Nothing
                [dir]
                `shouldReturn` (ignored, expected)
          )
      it "records unnamed patterns correctly" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "**/*", Named),
                    ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Unnamed "*unnamed*"),
                    ("unnamed 2.txt", "**/*", Unnamed "*unnamed*"),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "also ignore this", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Unnamed "**/*unnamed*")
                  ]
              loadFiles
                (makePatterns False ["**/*"])
                Nothing
                (Just $ makePatterns False ["*unnamed*", "**/*unnamed*"])
                [dir]
                `shouldReturn` (M.empty, expected)
          )
      it "successfully dedupes files by absolute path" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "**/*", Named),
                    ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Named),
                    ("unnamed 2.txt", "**/*", Named),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "also ignore this", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Named)
                  ]
              loadFiles
                (makePatterns False ["**/*"])
                Nothing
                Nothing
                [dir </> "subdirectory", dir]
                `shouldReturn` (M.empty, expected)
          )
      it "keeps unnamed if multiply input" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "**/*", Named),
                    ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Unnamed "./unnamed*"),
                    ("unnamed 2.txt", "**/*", Unnamed "./unnamed*"),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "also ignore this", "**/*", Named)
                  ]
              additionalExpected <-
                expectFiles
                  (dir </> "subdirectory")
                  [("unnamed 9.txt", "**/*", Unnamed "./unnamed*")]
              loadFiles
                (makePatterns False ["**/*"])
                Nothing
                (Just $ makePatterns False ["./unnamed*"])
                [dir </> "subdirectory", dir]
                `shouldReturn` (M.empty, M.union expected additionalExpected)
          )
      it "ignores a file if it is both ignored and input" $
        withSystemTempDirectory
          "autotagicalTest"
          ( \dir -> do
              prepareTestFiles dir
              ignored <-
                expectIgnoredFiles
                  dir
                  [ ("subdirectory" </> "also ignore this", "**/*", Named, "./subdirectory/*ignore*")
                  ]
              expected <-
                expectFiles
                  dir
                  [ ("ignore this", "**/*", Named),
                    ("anotherFile.txt", "**/*", Named),
                    ("unnamed 1.txt", "**/*", Named),
                    ("unnamed 2.txt", "**/*", Named),
                    ("subdirectory" </> "a file", "**/*", Named),
                    ("subdirectory" </> "unnamed 9.txt", "**/*", Named)
                  ]
              loadFiles
                (makePatterns False ["**/*"])
                (Just $ makePatterns False ["./subdirectory/*ignore*"])
                Nothing
                [dir </> "subdirectory", dir]
                `shouldReturn` (ignored, expected)
          )
    describe "renameFiles" $ do
      let inputFormat = InputFormat NameTagsExtension Whitespace Forbidden (Keyword ",") Nothing Forbidden Whitespace
      it "renames files correctly when there are no collisions and ignores named" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileOriginalName, FileTextLiteral "1"] Nothing) Always]
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ( "a",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "a" Nothing,
                        ""
                      )
                    ),
                    ( "b",
                      ( InputInfo "" "" Named,
                        FileInfo (Tags M.empty) "raw" "b" Nothing,
                        ""
                      )
                    ),
                    ( "c",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "c" Nothing,
                        ""
                      )
                    )
                  ]
              )
              `shouldBe` ( M.empty,
                           M.fromList
                             [ ("a", ("", "a1raw")),
                               ("b", ("", "braw")),
                               ("c", ("", "c1raw"))
                             ]
                         )
      it "correctly fails files that fail the initial rename" $
        let renamingSchema = RenamingSchema [RenamingRule (FileName [FileOriginalName, FileTextLiteral "1"] Nothing) (HasTag "tag" Nothing)]
         in renameFiles
              inputFormat
              Nothing
              (Just renamingSchema)
              ( M.fromList
                  [ ( "a",
                      ( simpleInputInfo,
                        FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw" "a" Nothing,
                        ""
                      )
                    ),
                    ( "b",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "b" Nothing,
                        ""
                      )
                    ),
                    ( "c",
                      ( simpleInputInfo,
                        FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw" "c" Nothing,
                        ""
                      )
                    )
                  ]
              )
              `shouldBe` ( M.singleton "b"
                             . RenamingError
                             $ NoRenamingMatch (FileInfo (Tags M.empty) "raw" "b" Nothing),
                           M.fromList
                             [("a", ("", "a1raw")), ("c", ("", "c1raw"))]
                         )
      it "renames files correctly when there are duplicates" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "a", FileIfDuplicate [FileDuplicateNumber]] Nothing) Always]
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ( "a",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "a" Nothing,
                        ""
                      )
                    ),
                    ( "b",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "b" Nothing,
                        ""
                      )
                    ),
                    ( "c",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw" "c" Nothing,
                        ""
                      )
                    )
                  ]
              )
              `shouldBe` ( M.empty,
                           M.fromList
                             [ ("a", ("", "a1raw")),
                               ("b", ("", "a2raw")),
                               ("c", ("", "a3raw"))
                             ]
                         )
      it "determines duplicates based on name+ext, not tags" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "a", FileIfDuplicate [FileDuplicateNumber]] Nothing) Always]
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ( "a",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw1" "a" (Just ".jpg"),
                        ""
                      )
                    ),
                    ( "b",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw2" "b" (Just ".png"),
                        ""
                      )
                    ),
                    ( "c",
                      ( simpleInputInfo,
                        FileInfo (Tags M.empty) "raw3" "c" (Just ".jpg"),
                        ""
                      )
                    )
                  ]
              )
              `shouldBe` ( M.empty,
                           M.fromList
                             [ ("a", ("", "a1raw1.jpg")),
                               ("b", ("", "araw2.png")),
                               ("c", ("", "a2raw3.jpg"))
                             ]
                         )
      it "fails all duplicates if one is not Unnamed" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "b", FileDuplicateNumber] Nothing) Always]
            colInfo = FileInfo (Tags M.empty) "raw" "b" Nothing
            ts = Tags $ M.fromList [(Tag "tag", Present)]
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ( "a",
                      ( simpleInputInfo,
                        FileInfo ts "raw" "a" Nothing,
                        ""
                      )
                    ),
                    ( "b",
                      (InputInfo "" "" Named, colInfo, "")
                    ),
                    ( "c",
                      ( simpleInputInfo,
                        FileInfo ts "raw" "c" Nothing,
                        ""
                      )
                    )
                  ]
              )
              `shouldBe` ( M.fromList
                             [ ( "a",
                                 CollisionBetweenUnnamedAndNamed
                                   colInfo
                                   (FileInfo ts "raw" "a" Nothing)
                                   ""
                                   "b"
                               ),
                               ( "b",
                                 CollisionBetweenUnnamedAndNamed
                                   colInfo
                                   colInfo
                                   ""
                                   "b"
                               ),
                               ( "c",
                                 CollisionBetweenUnnamedAndNamed
                                   colInfo
                                   (FileInfo ts "raw" "c" Nothing)
                                   ""
                                   "b"
                               )
                             ],
                           M.empty
                         )
      it "fails all duplicates if one fails to rename" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "a", FileIfDuplicate [FileTagValue "tag", FileDuplicateNumber]] Nothing) Always]
            ts = Tags $ M.fromList [(Tag "tag", Present)]
            f1 = FileInfo ts "raw" "a" Nothing
            f2 = FileInfo (Tags M.empty) "raw" "b" Nothing
            f3 = FileInfo ts "raw" "c" Nothing
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ("a", (simpleInputInfo, f1, "")),
                    ("b", (simpleInputInfo, f2, "")),
                    ("c", (simpleInputInfo, f3, ""))
                  ]
              )
              `shouldBe` ( M.fromList
                             [ ( "a",
                                 DuplicateFileFailedRename Nothing f1 "" "a"
                               ),
                               ( "b",
                                 DuplicateFileFailedRename
                                   (Just . RenamingError $ Renaming.NameTemplateErrors f2 [TagValueWithoutTag (Tag "tag")])
                                   f2
                                   ""
                                   "a"
                               ),
                               ( "c",
                                 DuplicateFileFailedRename Nothing f3 "" "a"
                               )
                             ],
                           M.empty
                         )
      it "fails all duplicates if one collides post-deduplication with a previously deduplicated file" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "a", FileIfDuplicate [FileTagValue "tag1", FileDuplicateNumber]] Nothing) (HasTag "tag1" Nothing), RenamingRule (FileName [FileTextLiteral "aa", FileDuplicateNumber] Nothing) (HasTag "tag2" Nothing)]
            ts = Tags $ M.fromList [(Tag "tag2", Present)]
            f1 = FileInfo (Tags $ M.fromList [(Tag "tag1", Value "a")]) "raw" "a" Nothing
            f3 = FileInfo (Tags $ M.fromList [(Tag "tag1", Present)]) "raw" "c" Nothing
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ("a", (simpleInputInfo, f1, "")),
                    ("b", (simpleInputInfo, FileInfo ts "raw" "b" Nothing, "")),
                    ("c", (simpleInputInfo, f3, "")),
                    ("d", (simpleInputInfo, FileInfo ts "raw" "d" Nothing, ""))
                  ]
              )
              `shouldBe` ( M.fromList
                             [ ( "a",
                                 CollisionBetweenDifferentRenamed f1 "" "a" "aa1" "aa1"
                               ),
                               ( "c",
                                 CollisionBetweenDifferentRenamed f3 "" "a" "atag12" "aa1"
                               )
                             ],
                           M.fromList
                             [ ("b", ("", "aa1raw")),
                               ("d", ("", "aa2raw"))
                             ]
                         )
      it "fails all duplicates if they collide after renaming" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTextLiteral "a", FileIfDuplicate [FileTagValue "tag"]] Nothing) Always]
            f1 = FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw1" "a" Nothing
            f2 = FileInfo (Tags $ M.fromList [(Tag "tag", Value "val")]) "raw2" "b" Nothing
            f3 = FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw3" "c" Nothing
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ("a", (simpleInputInfo, f1, "")),
                    ("b", (simpleInputInfo, f2, "")),
                    ("c", (simpleInputInfo, f3, ""))
                  ]
              )
              `shouldBe` ( M.fromList
                             [ ( "a",
                                 CollisionBetweenRenamedDuplicates f1 "" "a" "atag"
                               ),
                               ( "b",
                                 CollisionBetweenRenamedDuplicates f2 "" "a" "aval"
                               ),
                               ( "c",
                                 CollisionBetweenRenamedDuplicates f3 "" "a" "atag"
                               )
                             ],
                           M.empty
                         )
      it "fails all duplicates if one collides with a non-duplicate file" $
        let renamingSchema = Just $ RenamingSchema [RenamingRule (FileName [FileTagValue "tag", FileIfDuplicate [FileTextLiteral "val", FileDuplicateNumber]] Nothing) Always]
            f1 = FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw" "a" Nothing
            f3 = FileInfo (Tags $ M.fromList [(Tag "tag", Present)]) "raw" "c" Nothing
         in renameFiles
              inputFormat
              Nothing
              renamingSchema
              ( M.fromList
                  [ ("a", (simpleInputInfo, f1, "")),
                    ("b", (simpleInputInfo, FileInfo (Tags $ M.fromList [(Tag "tag", Value "tagval1")]) "raw" "b" Nothing, "")),
                    ("c", (simpleInputInfo, f3, ""))
                  ]
              )
              `shouldBe` ( M.fromList
                             [ ( "a",
                                 CollisionBetweenRenamedAndInitial f1 "" "tag" "tagval1" "tagval1"
                               ),
                               ( "c",
                                 CollisionBetweenRenamedAndInitial f3 "" "tag" "tagval2" "tagval1"
                               )
                             ],
                           M.singleton "b" ("", "tagval1raw")
                         )
    describe "determineMoves" $ do
      it "simply transfers 1-step, normal moves" $
        determineMoves
          ( M.fromList
              [ ("a", ("out1", "a")),
                ("a2", ("out2", "a")),
                ("a3", ("out2", "b"))
              ]
          )
          `shouldBe` ( M.fromList
                         [ ("a", "out1" </> "a"),
                           ("a2", "out2" </> "a"),
                           ("a3", "out2" </> "b")
                         ],
                       M.empty
                     )
      it "filters in-place moves" $
        determineMoves
          ( M.fromList
              [ ("out1" </> "a", ("out1", "a")),
                ("a2", ("out2", "a")),
                ("out2" </> "a3", ("out2", "a3"))
              ]
          )
          `shouldBe` (M.singleton "a2" $ "out2" </> "a", M.empty)
      it "successfully swaps files" $
        determineMoves
          ( M.fromList
              [ ("out" </> "a", ("out", "b")),
                ("a2", ("out2", "a")),
                ("out" </> "b", ("out", "a"))
              ]
          )
          `shouldSatisfy` \(f, s) ->
            case M.assocs f of
              [(i0, o0), (i1, t1), (i2, t2)] ->
                i0 == "a2"
                  && o0 == "out2" </> "a"
                  && i1 == "out" </> "a"
                  && i2 == "out" </> "b"
                  && M.lookup t1 s == Just ("out" </> "b")
                  && M.lookup t2 s == Just ("out" </> "a")
              _ -> False
    describe "copyFileToPath" $ do
      it "moves files when successful" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                setLogLevel LevelError
                writeFile (dir </> "fileIn") "test"
                copyFileToPath
                  False
                  (ClobberDestination False)
                  (dir </> "fileIn")
                  (dir </> "fileOut")
                  `shouldReturn` V.Success ()
                readFile (dir </> "fileIn") `shouldReturn` "test"
                readFile (dir </> "fileOut") `shouldReturn` "test"
            )
      it "moves files when it is necessary to create folders successfully" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                setLogLevel LevelError
                writeFile (dir </> "fileIn") "test"
                copyFileToPath
                  False
                  (ClobberDestination False)
                  (dir </> "fileIn")
                  (dir </> "subDir" </> "subSubDir" </> "fileOut")
                  `shouldReturn` V.Success ()
                readFile (dir </> "fileIn") `shouldReturn` "test"
                readFile (dir </> "subDir" </> "subSubDir" </> "fileOut")
                  `shouldReturn` "test"
            )
      it "fails when destination exists and clobber is false" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                writeFile (dir </> "fileOut") "test2"
                copyFileToPath
                  False
                  (ClobberDestination False)
                  (dir </> "fileIn")
                  (dir </> "fileOut")
                  `shouldReturn` V.Failure
                    [DestinationFileExists (dir </> "fileIn") (dir </> "fileOut")]
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                readFile (dir </> "fileOut") `shouldReturn` "test2"
            )
      it "clobbers files when destination exists and specified" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                writeFile (dir </> "fileOut") "test2"
                copyFileToPath
                  False
                  (ClobberDestination True)
                  (dir </> "fileIn")
                  (dir </> "fileOut")
                  `shouldReturn` V.Success ()
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                readFile (dir </> "fileOut") `shouldReturn` "test1"
            )
      it "fails files when destination exists and is a folder without clobber" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                createDirectory (dir </> "fileOut")
                copyFileToPath
                  False
                  (ClobberDestination False)
                  (dir </> "fileIn")
                  (dir </> "fileOut")
                  `shouldReturn` V.Failure [DirectoryInDestination (dir </> "fileIn") (dir </> "fileOut")]
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                doesDirectoryExist (dir </> "fileOut") `shouldReturn` True
            )
      it "fails files when destination exists and is a folder with clobber" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                createDirectory (dir </> "fileOut")
                copyFileToPath
                  False
                  (ClobberDestination True)
                  (dir </> "fileIn")
                  (dir </> "fileOut")
                  `shouldReturn` V.Failure
                    [DirectoryInDestination (dir </> "fileIn") (dir </> "fileOut")]
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                doesDirectoryExist (dir </> "fileOut") `shouldReturn` True
            )
      context "when dry run is true" $ do
        it "does not move files when successful" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  setLogLevel LevelError
                  writeFile (dir </> "fileIn") "test"
                  copyFileToPath
                    True
                    (ClobberDestination False)
                    (dir </> "fileIn")
                    (dir </> "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test"
                  doesPathExist (dir </> "fileOut") `shouldReturn` False
              )
        it "does not move files when it is necessary to create folders successfully" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  setLogLevel LevelError
                  writeFile (dir </> "fileIn") "test"
                  copyFileToPath
                    True
                    (ClobberDestination False)
                    (dir </> "fileIn")
                    (dir </> "subDir" </> "subSubDir" </> "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test"
                  doesPathExist (dir </> "subDir" </> "subSubDir")
                    `shouldReturn` False
                  doesPathExist (dir </> "subDir" </> "subSubDir" </> "fileOut")
                    `shouldReturn` False
              )
        it "fails when destination exists and clobber is false" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  writeFile (dir </> "fileOut") "test2"
                  copyFileToPath
                    True
                    (ClobberDestination False)
                    (dir </> "fileIn")
                    (dir </> "fileOut")
                    `shouldReturn` V.Failure
                      [DestinationFileExists (dir </> "fileIn") (dir </> "fileOut")]
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  readFile (dir </> "fileOut") `shouldReturn` "test2"
              )
        it "does not clobber files when destination exists and specified" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  writeFile (dir </> "fileOut") "test2"
                  copyFileToPath
                    True
                    (ClobberDestination True)
                    (dir </> "fileIn")
                    (dir </> "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  readFile (dir </> "fileOut") `shouldReturn` "test2"
              )
        it "fails files when destination exists and is a folder without clobber" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  createDirectory (dir </> "fileOut")
                  copyFileToPath
                    True
                    (ClobberDestination False)
                    (dir </> "fileIn")
                    (dir </> "fileOut")
                    `shouldReturn` V.Failure [DirectoryInDestination (dir </> "fileIn") (dir </> "fileOut")]
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  doesDirectoryExist (dir </> "fileOut") `shouldReturn` True
              )
        it "fails files when destination exists and is a folder with clobber" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  createDirectory (dir </> "fileOut")
                  copyFileToPath
                    True
                    (ClobberDestination True)
                    (dir </> "fileIn")
                    (dir </> "fileOut")
                    `shouldReturn` V.Failure
                      [DirectoryInDestination (dir </> "fileIn") (dir </> "fileOut")]
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  doesDirectoryExist (dir </> "fileOut") `shouldReturn` True
              )
    describe "moveFile" $ do
      it "moves file to single output when successful and does not keep copy" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                setLogLevel LevelError
                writeFile (dir </> "fileIn") "test"
                moveFile
                  False
                  (ClobberDestination False)
                  False
                  [dir]
                  (dir </> "fileIn", "fileOut")
                  `shouldReturn` V.Success ()
                doesPathExist (dir </> "fileIn") `shouldReturn` False
                readFile (dir </> "fileOut") `shouldReturn` "test"
            )
      it "moves file to single output when successful and keeps copy" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test"
                moveFile
                  False
                  (ClobberDestination False)
                  True
                  [dir]
                  (dir </> "fileIn", "fileOut")
                  `shouldReturn` V.Success ()
                readFile (dir </> "fileIn") `shouldReturn` "test"
                readFile (dir </> "fileOut") `shouldReturn` "test"
            )
      it "moves file to multiple outputs when successful and does not keep copy" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test"
                moveFile
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  (dir </> "fileIn", "fileOut")
                  `shouldReturn` V.Success ()
                doesPathExist (dir </> "fileIn") `shouldReturn` False
                readFile (dir </> "dir1" </> "fileOut") `shouldReturn` "test"
                readFile (dir </> "dir2" </> "fileOut") `shouldReturn` "test"
            )
      it "does not remove original if at least one fails" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                createDirectory (dir </> "dir2")
                writeFile (dir </> "dir2" </> "fileOut") "test2"
                moveFile
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  (dir </> "fileIn", "fileOut")
                  `shouldReturn` V.Failure
                    [DestinationFileExists (dir </> "fileIn") (dir </> "dir2" </> "fileOut")]
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                readFile (dir </> "dir1" </> "fileOut") `shouldReturn` "test1"
                readFile (dir </> "dir2" </> "fileOut") `shouldReturn` "test2"
            )
      it "accumulates errors, rather than stopping at first" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn") "test1"
                createDirectory (dir </> "dir2")
                writeFile (dir </> "dir2" </> "fileOut") "test2"
                createDirectoryIfMissing True (dir </> "dir1" </> "fileOut")
                moveFile
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  (dir </> "fileIn", "fileOut")
                  `shouldReturn` V.Failure
                    [ DirectoryInDestination (dir </> "fileIn") (dir </> "dir1" </> "fileOut"),
                      DestinationFileExists (dir </> "fileIn") (dir </> "dir2" </> "fileOut")
                    ]
                readFile (dir </> "fileIn") `shouldReturn` "test1"
                doesDirectoryExist (dir </> "dir1" </> "fileOut")
                  `shouldReturn` True
                readFile (dir </> "dir2" </> "fileOut") `shouldReturn` "test2"
            )
      context "when dry run is true" $ do
        it "does not move file to single output when successful and does keep copy" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  setLogLevel LevelError
                  writeFile (dir </> "fileIn") "test"
                  moveFile
                    True
                    (ClobberDestination False)
                    False
                    [dir]
                    (dir </> "fileIn", "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test"
                  doesPathExist (dir </> "fileOut") `shouldReturn` False
              )
        it "does not move file to single output when successful and keeps copy" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test"
                  moveFile
                    True
                    (ClobberDestination False)
                    True
                    [dir]
                    (dir </> "fileIn", "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test"
                  doesPathExist (dir </> "fileOut") `shouldReturn` False
              )
        it "does not move file to multiple outputs when successful and keeps copy" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test"
                  moveFile
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    (dir </> "fileIn", "fileOut")
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn") `shouldReturn` "test"
                  doesPathExist (dir </> "dir1" </> "fileOut") `shouldReturn` False
                  doesPathExist (dir </> "dir2" </> "fileOut") `shouldReturn` False
              )
        it "does not remove original if at least one fails" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  createDirectory (dir </> "dir2")
                  writeFile (dir </> "dir2" </> "fileOut") "test2"
                  moveFile
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    (dir </> "fileIn", "fileOut")
                    `shouldReturn` V.Failure
                      [DestinationFileExists (dir </> "fileIn") (dir </> "dir2" </> "fileOut")]
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  doesPathExist (dir </> "dir1" </> "fileOut") `shouldReturn` False
                  readFile (dir </> "dir2" </> "fileOut") `shouldReturn` "test2"
              )
        it "accumulates errors, rather than stopping at first" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn") "test1"
                  createDirectory (dir </> "dir2")
                  writeFile (dir </> "dir2" </> "fileOut") "test2"
                  createDirectoryIfMissing True (dir </> "dir1" </> "fileOut")
                  moveFile
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    (dir </> "fileIn", "fileOut")
                    `shouldReturn` V.Failure
                      [ DirectoryInDestination (dir </> "fileIn") (dir </> "dir1" </> "fileOut"),
                        DestinationFileExists (dir </> "fileIn") (dir </> "dir2" </> "fileOut")
                      ]
                  readFile (dir </> "fileIn") `shouldReturn` "test1"
                  doesDirectoryExist (dir </> "dir1" </> "fileOut")
                    `shouldReturn` True
                  readFile (dir </> "dir2" </> "fileOut") `shouldReturn` "test2"
              )
    describe "moveFiles" $ do
      it "moves files to single output when successful and does not keep copy" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                setLogLevel LevelError
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                moveFiles
                  False
                  (ClobberDestination False)
                  False
                  [dir]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Success ()
                doesPathExist (dir </> "fileIn1") `shouldReturn` False
                doesPathExist (dir </> "fileIn2") `shouldReturn` False
                readFile (dir </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "fileOut2") `shouldReturn` "test2"
            )
      it "moves files to single output when successful and keeps copies" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                moveFiles
                  False
                  (ClobberDestination False)
                  True
                  [dir]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Success ()
                readFile (dir </> "fileIn1") `shouldReturn` "test1"
                readFile (dir </> "fileIn2") `shouldReturn` "test2"
                readFile (dir </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "fileOut2") `shouldReturn` "test2"
            )
      it "moves file to multiple outputs when successful and does not keep copy" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                moveFiles
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Success ()
                doesPathExist (dir </> "fileIn1") `shouldReturn` False
                doesPathExist (dir </> "fileIn2") `shouldReturn` False
                readFile (dir </> "dir1" </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "dir1" </> "fileOut2") `shouldReturn` "test2"
                readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "dir2" </> "fileOut2") `shouldReturn` "test2"
            )
      it "fails a single file if any output fails and keeps a copy, while processing the rest correctly" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                createDirectory (dir </> "dir2")
                writeFile (dir </> "dir2" </> "fileOut1") "test3"
                moveFiles
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Failure
                    [DestinationFileExists (dir </> "fileIn1") (dir </> "dir2" </> "fileOut1")]
                readFile (dir </> "fileIn1") `shouldReturn` "test1"
                doesPathExist (dir </> "fileIn2") `shouldReturn` False
                readFile (dir </> "dir1" </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "dir1" </> "fileOut2") `shouldReturn` "test2"
                readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test3"
                readFile (dir </> "dir2" </> "fileOut2") `shouldReturn` "test2"
            )
      it "clobbers when specified" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                createDirectory (dir </> "dir2")
                writeFile (dir </> "dir2" </> "fileOut1") "test3"
                moveFiles
                  False
                  (ClobberDestination True)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Success ()
                doesPathExist (dir </> "fileIn1") `shouldReturn` False
                doesPathExist (dir </> "fileIn2") `shouldReturn` False
                readFile (dir </> "dir1" </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "dir1" </> "fileOut2") `shouldReturn` "test2"
                readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test1"
                readFile (dir </> "dir2" </> "fileOut2") `shouldReturn` "test2"
            )
      it "accumulates multiple errors" $
        withStdoutLogging $
          withSystemTempDirectory
            "autotagicalTest"
            ( \dir -> do
                writeFile (dir </> "fileIn1") "test1"
                writeFile (dir </> "fileIn2") "test2"
                createDirectory (dir </> "dir1")
                writeFile (dir </> "dir1" </> "fileOut1") "test3"
                createDirectory (dir </> "dir2")
                writeFile (dir </> "dir2" </> "fileOut1") "test4"
                writeFile (dir </> "dir2" </> "fileOut2") "test5"
                moveFiles
                  False
                  (ClobberDestination False)
                  False
                  [dir </> "dir1", dir </> "dir2"]
                  ( M.fromList
                      [ (dir </> "fileIn1", "fileOut1"),
                        (dir </> "fileIn2", "fileOut2")
                      ]
                  )
                  `shouldReturn` V.Failure
                    [ DestinationFileExists (dir </> "fileIn1") (dir </> "dir1" </> "fileOut1"),
                      DestinationFileExists (dir </> "fileIn1") (dir </> "dir2" </> "fileOut1"),
                      DestinationFileExists (dir </> "fileIn2") (dir </> "dir2" </> "fileOut2")
                    ]
                readFile (dir </> "fileIn1") `shouldReturn` "test1"
                readFile (dir </> "fileIn2") `shouldReturn` "test2"
                readFile (dir </> "dir1" </> "fileOut1") `shouldReturn` "test3"
                readFile (dir </> "dir1" </> "fileOut2") `shouldReturn` "test2"
                readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test4"
                readFile (dir </> "dir2" </> "fileOut2") `shouldReturn` "test5"
            )
      context "when dry run is true" $ do
        it "does not move files to single output when successful and keeps copy" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  setLogLevel LevelError
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  moveFiles
                    True
                    (ClobberDestination False)
                    False
                    [dir]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  doesPathExist (dir </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "fileOut2") `shouldReturn` False
              )
        it "does not move files to single output when successful and keeps copies" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  moveFiles
                    True
                    (ClobberDestination False)
                    True
                    [dir]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  doesPathExist (dir </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "fileOut2") `shouldReturn` False
              )
        it "does not move file to multiple outputs when successful and keeps copy" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  moveFiles
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  doesPathExist (dir </> "dir1" </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "dir1" </> "fileOut2") `shouldReturn` False
                  doesPathExist (dir </> "dir2" </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "dir2" </> "fileOut2") `shouldReturn` False
              )
        it "fails a single file if any output fails and keeps a copy, while processing the rest correctly" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  createDirectory (dir </> "dir2")
                  writeFile (dir </> "dir2" </> "fileOut1") "test3"
                  moveFiles
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Failure
                      [DestinationFileExists (dir </> "fileIn1") (dir </> "dir2" </> "fileOut1")]
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  doesPathExist (dir </> "dir1" </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "dir1" </> "fileOut2") `shouldReturn` False
                  readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test3"
                  doesPathExist (dir </> "dir2" </> "fileOut2") `shouldReturn` False
              )
        it "does not clobber when specified" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  createDirectory (dir </> "dir2")
                  writeFile (dir </> "dir2" </> "fileOut1") "test3"
                  moveFiles
                    True
                    (ClobberDestination True)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Success ()
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  doesPathExist (dir </> "dir1" </> "fileOut1") `shouldReturn` False
                  doesPathExist (dir </> "dir1" </> "fileOut2") `shouldReturn` False
                  readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test3"
                  doesPathExist (dir </> "dir2" </> "fileOut2") `shouldReturn` False
              )
        it "accumulates multiple errors" $
          withStdoutLogging $
            withSystemTempDirectory
              "autotagicalTest"
              ( \dir -> do
                  writeFile (dir </> "fileIn1") "test1"
                  writeFile (dir </> "fileIn2") "test2"
                  createDirectory (dir </> "dir1")
                  writeFile (dir </> "dir1" </> "fileOut1") "test3"
                  createDirectory (dir </> "dir2")
                  writeFile (dir </> "dir2" </> "fileOut1") "test4"
                  writeFile (dir </> "dir2" </> "fileOut2") "test5"
                  moveFiles
                    True
                    (ClobberDestination False)
                    False
                    [dir </> "dir1", dir </> "dir2"]
                    ( M.fromList
                        [ (dir </> "fileIn1", "fileOut1"),
                          (dir </> "fileIn2", "fileOut2")
                        ]
                    )
                    `shouldReturn` V.Failure
                      [ DestinationFileExists (dir </> "fileIn1") (dir </> "dir1" </> "fileOut1"),
                        DestinationFileExists (dir </> "fileIn1") (dir </> "dir2" </> "fileOut1"),
                        DestinationFileExists (dir </> "fileIn2") (dir </> "dir2" </> "fileOut2")
                      ]
                  readFile (dir </> "fileIn1") `shouldReturn` "test1"
                  readFile (dir </> "fileIn2") `shouldReturn` "test2"
                  readFile (dir </> "dir1" </> "fileOut1") `shouldReturn` "test3"
                  doesPathExist (dir </> "dir1" </> "fileOut2") `shouldReturn` False
                  readFile (dir </> "dir2" </> "fileOut1") `shouldReturn` "test4"
                  readFile (dir </> "dir2" </> "fileOut2") `shouldReturn` "test5"
              )
