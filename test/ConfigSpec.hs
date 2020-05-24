{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec
  ( spec,
  )
where

import Config (AutotagicalConfig (..))
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import FileHandler (ClobberDestination (..))
import Logging (LogDestination (..), LogLevel (..))
import NameTemplate
  ( FileName (..),
    FileNameComponent (..),
    FolderName (..),
    FolderNameComponent (..),
  )
import Parser
  ( InputFormat (..),
    Order (..),
    OutputFormat (..),
    Separator (..),
    SeparatorRequirement (..),
  )
import Predicate (Predicate (..))
import Renaming (RenamingRule (..), RenamingSchema (..))
import Sorting (Folder (..), SortingSchema (..))
import Test.Hspec
import Utility (importSucceeds, makePatterns)

spec :: Spec
spec = parallel
  $ describe "AutotagicalConfig"
  $ do
    let d = autoWith defaultInputNormalizer :: Decoder AutotagicalConfig
    it "imports without renaming or output format" $
      importSucceeds
        d
        ( AutotagicalConfig
            (ClobberDestination False)
            Nothing
            ["input"]
            ( InputFormat
                NameTagsExtension
                (Keyword "[")
                Optional
                Whitespace
                Nothing
                Optional
                (Keyword "]")
            )
            (makePatterns False ["*.jpg", "*/*.png", "literal"])
            True
            StdOut
            Warn
            ["output"]
            Nothing
            Nothing
            ( SortingSchema
                [ Folder
                    (FolderName [FolderTextLiteral "test1"] Nothing)
                    Always
                    [ Folder
                        (FolderName [FolderTextLiteral "test2"] Nothing)
                        (Not Always)
                        []
                    ]
                ]
            )
        )
        "autotagicalConfig-noOutputRenaming.dhall"
    it "imports with renaming and output format" $
      importSucceeds
        d
        ( AutotagicalConfig
            (ClobberDestination False)
            (Just $ makePatterns False [".*"])
            ["input"]
            ( InputFormat
                NameTagsExtension
                (Keyword "[")
                Optional
                Whitespace
                Nothing
                Optional
                (Keyword "]")
            )
            (makePatterns False ["*.jpg", "*/*.png", "literal"])
            False
            StdOut
            Warn
            ["output"]
            ( Just $
                OutputFormat NameTagsExtension "{" False "," Nothing False "}"
            )
            ( Just
                ( makePatterns True ["unnamed*"],
                  RenamingSchema
                    [ RenamingRule
                        (FileName [FileTextLiteral "rename"] Nothing)
                        Always
                    ]
                )
            )
            ( SortingSchema
                [ Folder
                    (FolderName [FolderTextLiteral "test1"] Nothing)
                    Always
                    [ Folder
                        (FolderName [FolderTextLiteral "test2"] Nothing)
                        (Not Always)
                        []
                    ]
                ]
            )
        )
        "autotagicalConfig-withOutputRenaming.dhall"
