{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module TagSpec
  ( spec,
  )
where

import Dhall (Decoder, autoWith, defaultInputNormalizer)
import SafeType (makeNonEmptySet)
import Tag (Tag, TagGroup (..))
import Test.Hspec
import Utility (importFails, importSucceeds)

spec :: Spec
spec = parallel
  $ context "FromDhall instances"
  $ do
    describe "Tag" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Tag
      it "imports when valid" $
        importSucceeds d "test" "tag.dhall"
      it "fails when text is empty" $
        importFails d "tag-empty.dhall"
      it "fails when text contains a slash" $
        importFails d "tag-slash.dhall"
      it "fails when text not printable" $
        importFails d "tag-unprintable.dhall"
    describe "TagGroup" $ do
      let d = autoWith defaultInputNormalizer :: Decoder TagGroup
      it "imports when valid" $
        importSucceeds d (TagGroup $ makeNonEmptySet ["test1", "test2"]) "tagGroup.dhall"
      it "imports when duplicate tags" $
        importSucceeds d (TagGroup $ makeNonEmptySet ["test"]) "tagGroup-duplicate.dhall"
      it "imports TagSpaces v3.4.2 Json" $
        importSucceeds d (TagGroup $ makeNonEmptySet ["tag1", "tag2"]) "tagGroup-tagSpaces-v3.4.2.dhall"
      it "fails when specified TagSpaces v3 group is empty string" $
        importFails d "tagGroup-tagSpaces-v3-emptySpecifiedGroup.dhall"
      it "fails when specified TagSpaces v3 group contains no tags" $
        importFails d "tagGroup-tagSpaces-v3-emptyGroup.dhall"
      it "imports fails when TagSpaces v3 Json is invalid" $
        importFails d "tagGroup-tagSpaces-v3-invalid.dhall"
      it "fails when specified TagSpaces v3 group is not found" $
        importFails d "tagGroup-tagSpaces-v3-missingGroup.dhall"
      it "fails when specified TagSpaces v3 group contains an invalid tag" $
        importFails d "tagGroup-tagSpaces-v3-invalidTag.dhall"
      it "fails when a tag is invalid" $
        importFails d "tagGroup-invalid.dhall"
      it "fails when no tags are provided" $
        importFails d "tagGroup-empty.dhall"
