{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SafeTypeSpec
  ( spec,
  )
where

import Dhall (Decoder, autoWith, defaultInputNormalizer)
import SafeType (NonEmptySet, SafeText, makeNonEmptySet)
import Test.Hspec
import Utility (importFails, importSucceeds)

spec :: Spec
spec =
  context "FromDhall instances" $ do
    describe "SafeText" $ do
      let d = autoWith defaultInputNormalizer :: Decoder SafeText
      it "imports when valid" $
        importSucceeds d "safe" "safeText.dhall"
      it "fails when a slash" $
        importFails d "safeText-slash.dhall"
      it "fails when empty" $
        importFails d "safeText-empty.dhall"
      it "fails when not a printable character" $
        importFails d "safeText-unprintable.dhall"
    describe "NonEmptySet" $ do
      let d = autoWith defaultInputNormalizer :: Decoder (NonEmptySet SafeText)
      it "imports when valid" $
        importSucceeds d (makeNonEmptySet ["test1", "test2"]) "nonEmptySet.dhall"
      it "fails when list is empty" $
        importFails d "nonEmptySet-empty.dhall"
