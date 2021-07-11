{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SortingSpec
  ( spec,
  )
where

import Arbitrary ()
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import NameTemplate
  ( FolderName (..),
    FolderNameComponent (..),
    NameTemplateError (..),
  )
import Predicate (Predicate (..), checkPredicate)
import Sorting
  ( Folder (..),
    SortingError (..),
    SortingSchema (..),
    sortBySchema,
    sortToFolder,
  )
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import Utility (importFails, importSucceeds)

spec :: Spec
spec = do
  context "FromDhall instances" $ do
    describe "Folder" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Folder
      it "imports successfully with no subfolders" $
        importSucceeds
          d
          ( Folder
              (FolderName [FolderTextLiteral "test"] Nothing)
              Always
              []
          )
          "folder-leaf.dhall"
      it "imports successfully with no subfolders and template" $
        importSucceeds
          d
          ( Folder
              (FolderName [FolderTextLiteral "test"] (Just " "))
              Always
              []
          )
          "folder-leafWithTemplate.dhall"
      it "imports successfully with multiple levels of subfolder" $
        importSucceeds
          d
          ( Folder
              (FolderName [FolderTextLiteral "parent"] Nothing)
              Always
              [ Folder
                  (FolderName [FolderTextLiteral "child1"] Nothing)
                  (Not Always)
                  [],
                Folder
                  (FolderName [FolderTextLiteral "child2"] Nothing)
                  (Not Always)
                  [ Folder
                      (FolderName [FolderTextLiteral "grandchild"] Nothing)
                      Always
                      []
                  ]
              ]
          )
          "folder-subfolders.dhall"
    describe "SortingSchema" $ do
      let d = autoWith defaultInputNormalizer :: Decoder SortingSchema
      it "imports successfully" $
        importSucceeds
          d
          ( SortingSchema
              [ Folder
                  (FolderName [FolderTextLiteral "test1"] Nothing)
                  Always
                  [],
                Folder
                  (FolderName [FolderTextLiteral "test2"] Nothing)
                  Always
                  [ Folder
                      (FolderName [FolderTextLiteral "child"] Nothing)
                      Always
                      []
                  ]
              ]
          )
          "sortingSchema.dhall"
      it "fails when empty" $
        importFails d "sortingSchema-empty.dhall"
  parallel $ do
    describe "sortToFolder" $ do
      prop "always sorts to the first valid folder" $
        \ts n fs ->
          sortToFolder ts (Folder n Always [] : fs)
            `shouldBe` Just [Folder n Always []]
      prop "never sorts to an invalid folder" $
        \ts fs ->
          sortToFolder ts fs
            `shouldSatisfy` (all (checkPredicate ts . (predicate :: Folder -> Predicate)) . fromMaybe [])
      prop "errors out when appropriate" $
        \ts n ->
          sortToFolder ts [Folder n (Not Always) []] `shouldBe` Nothing
    describe "sortBySchema" $ do
      prop "errors out when no match found" $
        \f n ->
          sortBySchema (SortingSchema [Folder n (Not Always) []]) f
            `shouldBe` Left (NoSortingMatch f)
      prop "errors out when name template does" $
        \f ->
          sortBySchema
            ( SortingSchema
                [ Folder
                    (FolderName [FolderIfThenElse Always [] []] Nothing)
                    Always
                    []
                ]
            )
            f
            `shouldBe` Left
              ( NameTemplateErrors
                  f
                  [ NullNameTemplate . T.pack . show $
                      FolderName [FolderIfThenElse Always [] []] Nothing
                  ]
              )
      prop "returns the appropriate name when valid" $
        \f n ->
          sortBySchema
            ( SortingSchema
                [ Folder
                    (FolderName [FolderTextLiteral "parent"] Nothing)
                    Always
                    [ Folder n (Not Always) [],
                      Folder
                        ( FolderName
                            [FolderTextLiteral "child", FolderTextLiteral "2"]
                            (Just " ")
                        )
                        Always
                        []
                    ]
                ]
            )
            f
            `shouldBe` Right ("parent" </> "child 2")
