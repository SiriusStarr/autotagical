{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec
  ( spec,
  )
where

import Arbitrary ()
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import Parser
  ( FileInfo (..),
    FileParser (..),
    InputFormat (..),
    Order (..),
    OutputFormat (..),
    OutputTaggedValueFormat (..),
    Parser,
    Separator (..),
    SeparatorRequirement (..),
    TagValueOrder (..),
    TaggedValueFormat (..),
    fileParser,
    parseFileInfo,
    separatorParser,
    tagParser,
    taggedValueParser,
  )
import Renaming (sepToText)
import SafeType (safeText)
import Tag (Tag (..), TagValue (..), Tags (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec (eof, parseMaybe)
import Utility (importFails, importSucceeds, validateTaggedValue)

-- | Generate non-empty whitespace for testing Whitespace Separators
genNonEmptyWhitespace :: Gen Text
genNonEmptyWhitespace =
  T.pack
    <$> listOf1 (elements [' ', '\t', '\n', '\r', '\f', '\v'])

-- | Shorthand for successful parsers
parserSucceeds :: Show a => Parser a -> Text -> Expectation
parserSucceeds p t =
  parseMaybe p t `shouldSatisfy` \case
    Just _ ->
      True
    Nothing ->
      False

-- | Shorthand for failing parsers
parserFails :: Show a => Parser a -> Text -> Expectation
parserFails p t =
  parseMaybe p t `shouldSatisfy` \case
    Just _ ->
      False
    Nothing ->
      True

spec :: Spec
spec = do
  context "FromDhall instances" $ do
    describe "Order" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Order
      it "imports successfully when NameTagsExtension" $
        importSucceeds d NameTagsExtension "order-nameTagsExtension.dhall"
      it "imports successfully when TagsNameExtension" $
        importSucceeds d TagsNameExtension "order-tagsNameExtension.dhall"
    describe "TagValueOrder" $ do
      let d = autoWith defaultInputNormalizer :: Decoder TagValueOrder
      it "imports successfully when TagBeforeValue" $
        importSucceeds d TagBeforeValue "tagValueOrder-tagBeforeValue.dhall"
      it "imports successfully when ValueBeforeTag" $
        importSucceeds d ValueBeforeTag "tagValueOrder-valueBeforeTag.dhall"
    describe "SeparatorRequirement" $ do
      let d = autoWith defaultInputNormalizer :: Decoder [SeparatorRequirement]
      it "imports all successfully" $
        importSucceeds
          d
          [Forbidden, Optional, Required]
          "separatorRequirement.dhall"
    describe "Separator" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Separator
      it "imports when Whitespace" $
        importSucceeds d Whitespace "separator-whitespace.dhall"
      it "imports when Keyword" $
        importSucceeds d (Keyword "test") "separator-keyword.dhall"
      it "fails when Keyword is empty" $
        importFails d "separator-keyword-empty.dhall"
      it "fails when Keyword contains a slash" $
        importFails d "separator-keyword-slash.dhall"
    describe "TaggedValueFormat" $ do
      let d = autoWith defaultInputNormalizer :: Decoder TaggedValueFormat
      it "imports when TagsFirst" $
        importSucceeds
          d
          ( TaggedValueFormat
              TagBeforeValue
              Nothing
              Whitespace
              Nothing
              True
          )
          "taggedValueFormat-tagsFirst.dhall"
      it "imports when ValuesFirst" $
        importSucceeds
          d
          ( TaggedValueFormat
              ValueBeforeTag
              (Just $ Keyword "start")
              Whitespace
              (Just $ Keyword ",")
              False
          )
          "taggedValueFormat-valuesFirst.dhall"
      it "fails when a Separator is invalid" $
        importFails d "taggedValueFormat-invalidSeparator.dhall"
    describe "InputFormat" $ do
      let d = autoWith defaultInputNormalizer :: Decoder InputFormat
      it "imports when tagspaces format" $
        importSucceeds
          d
          ( InputFormat
              NameTagsExtension
              (Keyword "[")
              Optional
              Whitespace
              Nothing
              Optional
              (Keyword "]")
          )
          "inputFormat-nameTagsExtension.dhall"
      it "imports when another format" $
        importSucceeds
          d
          ( InputFormat
              TagsNameExtension
              Whitespace
              Optional
              (Keyword ",")
              ( Just $
                  TaggedValueFormat
                    TagBeforeValue
                    Nothing
                    (Keyword "=")
                    Nothing
                    True
              )
              Optional
              Whitespace
          )
          "inputFormat-tagsNameExtension.dhall"
      it "fails when separators could be confused by parser" $
        importFails d "inputFormat-confusableSeparators.dhall"
      it "fails when separators could be confused by parser in tagged value format" $
        importFails d "inputFormat-confusableTaggedValueSeparators.dhall"
    describe "OutputTaggedValueFormat" $ do
      let d = autoWith defaultInputNormalizer :: Decoder OutputTaggedValueFormat
      it "imports successfully with no separators (tags first)" $
        importSucceeds
          d
          ( OutputTaggedValueFormat
              TagBeforeValue
              Nothing
              "="
              Nothing
              True
          )
          "outputTaggedValueFormat-noSeparators.dhall"
      it "imports successfully with separators (values first)" $
        importSucceeds
          d
          ( OutputTaggedValueFormat
              ValueBeforeTag
              (Just "{")
              " "
              (Just "}")
              False
          )
          "outputTaggedValueFormat-separators.dhall"
      it "fails with empty separators" $
        importFails d "outputTaggedValueFormat-emptySeparator.dhall"
    describe "OutputFormat" $ do
      let d = autoWith defaultInputNormalizer :: Decoder OutputFormat
      it "imports successfully when name then tags" $
        importSucceeds
          d
          ( OutputFormat
              NameTagsExtension
              "["
              False
              " "
              Nothing
              False
              "]"
          )
          "outputFormat-nameTagsExtension.dhall"
      it "imports successfully when tags then name" $
        importSucceeds
          d
          ( OutputFormat
              TagsNameExtension
              "{"
              True
              "-"
              ( Just $
                  OutputTaggedValueFormat
                    TagBeforeValue
                    Nothing
                    "="
                    Nothing
                    True
              )
              True
              "}"
          )
          "outputFormat-tagsNameExtension.dhall"
      it "fails when separators could be confused by parser" $
        importFails d "outputFormat-confusableSeparators.dhall"
      it "fails when separators could be confused by parser in tagged value format" $
        importFails d "outputFormat-confusableTaggedValueSeparators.dhall"
  parallel $ do
    describe "separatorParser" $ do
      context "Whitespace" $ do
        prop "matches any amount of whitespace" $
          forAll genNonEmptyWhitespace $
            \t -> parserSucceeds (separatorParser Whitespace <* eof) t
        it "does not match empty text" $
          parserFails (separatorParser Whitespace) ""
        prop "does not match any text that is not entirely whitespace" $
          \t ->
            not (T.any isSpace (safeText t))
              ==> parserFails (separatorParser Whitespace <* eof)
              $ safeText t
      context "Keyword" $ do
        prop "matches the keyword itself" $
          \t -> parserSucceeds (separatorParser (Keyword t) <* eof) $ safeText t
        prop "does not match any other text" $
          \t1 t2 ->
            t1 /= t2
              ==> parserFails (separatorParser (Keyword t1) <* eof)
              $ safeText t2
    describe "taggedValueParser" $ do
      context "when Nothing" $
        prop "never returns any value but Present" $
          \t ->
            parseMaybe (taggedValueParser Nothing eof eof <* eof) (safeText t)
              `shouldBe` Just (Tag t, Present)
      context "when order is TagBeforeValue" $
        prop "parses tags first" $
          \pre t tv v post ->
            let f = Just $ TaggedValueFormat TagBeforeValue pre tv post False
             in validateTaggedValue f (Tag t) (Value v)
                  ==> parseMaybe
                    (taggedValueParser f eof eof <* eof)
                    ( T.concat
                        [ maybe "" sepToText pre,
                          safeText t,
                          sepToText tv,
                          safeText v,
                          maybe "" sepToText post
                        ]
                    )
                    `shouldBe` Just (Tag t, Value v)
      context "ValuesFirst" $
        prop "parses values first" $
          \pre v tv t post ->
            let f = Just $ TaggedValueFormat ValueBeforeTag pre tv post False
             in validateTaggedValue f (Tag t) (Value v)
                  ==> parseMaybe
                    (taggedValueParser f eof eof <* eof)
                    ( T.concat
                        [ maybe "" sepToText pre,
                          safeText v,
                          sepToText tv,
                          safeText t,
                          maybe "" sepToText post
                        ]
                    )
                    `shouldBe` Just (Tag t, Value v)
    describe "tagParser" $ do
      let p l tv t = tagParser Whitespace l (Keyword ",") tv t Whitespace <* eof
      context "pure tags are not allowed" $ do
        let p' = p Forbidden (Just $ TaggedValueFormat TagBeforeValue Nothing (Keyword "=") Nothing False) Required
        it "succeeds when only tagged values are encountered" $
          parserSucceeds p' " tag1=val1,tag2=val2,tag3=val3, "
        it "fails when non-tagged values are encountered" $
          parserFails p' " tag1=val1,tag2,tag3=val3, "
      context "pure tags are allowed" $ do
        let p' ord = p Forbidden (Just $ TaggedValueFormat ord Nothing (Keyword "=") Nothing True) Required
        it "succeeds when only tagged values are encountered when tags first" $
          parserSucceeds (p' TagBeforeValue) " tag1=val1,tag2=val2,tag3=val3, "
        it "succeeds when non-tagged values are encountered when tags first" $
          parserSucceeds (p' TagBeforeValue) " tag1=val1,tag2,tag3=val3, "
        it "succeeds when only non-tagged values when tags first" $
          parserSucceeds (p' TagBeforeValue) " tag1,tag2,tag3, "
        it "succeeds when only tagged values are encountered when values first" $
          parserSucceeds (p' ValueBeforeTag) " val1=tag1,val2=tag2,val3=tag3, "
        it "succeeds when non-tagged values are encountered when values first" $
          parserSucceeds (p' ValueBeforeTag) " val1=tag1,tag2,val3=tag3, "
        it "succeeds when only non-tagged values when values first" $
          parserSucceeds (p' ValueBeforeTag) " tag1,tag2,tag3, "
      context "trailing separator" $ do
        let p' = p Forbidden Nothing
        context "when Required" $ do
          it "succeeds with it" $
            parserSucceeds (p' Required) " tag1,tag2,tag3, "
          it "fails without it" $
            parserFails (p' Required) " tag1,tag2,tag3 "
        context "when Forbidden" $ do
          it "fails with it" $
            parserFails (p' Forbidden) " tag1,tag2,tag3, "
          it "succeeds without it" $
            parserSucceeds (p' Forbidden) " tag1,tag2,tag3 "
        context "when Optional" $ do
          it "succeeds with it" $
            parserSucceeds (p' Optional) " tag1,tag2,tag3, "
          it "succeeds without it" $
            parserSucceeds (p' Optional) " tag1,tag2,tag3 "
      context "leading separator" $ do
        let p' r = p r Nothing Optional
        context "when Required" $ do
          it "succeeds with it" $
            parserSucceeds (p' Required) " ,tag1,tag2,tag3, "
          it "fails without it" $
            parserFails (p' Required) " tag1,tag2,tag3, "
        context "when Forbidden" $ do
          it "fails with it" $
            parserFails (p' Forbidden) " ,tag1,tag2,tag3, "
          it "succeeds without it" $
            parserSucceeds (p' Forbidden) " tag1,tag2,tag3, "
        context "when Optional" $ do
          it "succeeds with it" $
            parserSucceeds (p' Optional) " ,tag1,tag2,tag3, "
          it "succeeds without it" $
            parserSucceeds (p' Optional) " tag1,tag2,tag3, "
    describe "fileParser" $ do
      let p o = parser . fileParser $ InputFormat o Whitespace Forbidden (Keyword ",") Nothing Forbidden Whitespace
      let f = (T.pack "FileName", (T.pack " tag1,tag2,tag3 ", Tags $ M.fromList [("tag1", Present), ("tag2", Present), ("tag3", Present)]))
      context "when order is NameTagExtension" $
        it "returns Name first" $
          parseMaybe (p NameTagsExtension) "FileName tag1,tag2,tag3 "
            `shouldBe` Just f
      context "when order is TagNameExtension" $
        it "returns Tag first" $
          parseMaybe (p TagsNameExtension) " tag1,tag2,tag3 FileName"
            `shouldBe` Just f
    describe "parseFileInfo" $ do
      let parseFile o = parseFileInfo o . fileParser $ InputFormat o Whitespace Forbidden (Keyword ",") Nothing Forbidden Whitespace
      let parseFileNoWS o t tv = parseFileInfo o . fileParser $ InputFormat o (Keyword "[") Forbidden Whitespace tv t (Keyword "]")
      it "works as NameTagsExtension" $
        parseFile NameTagsExtension "FileName tag1,tag2,tag3 .ext"
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag1", Present), ("tag2", Present), ("tag3", Present)]
                )
                " tag1,tag2,tag3 "
                "FileName"
                (Just ".ext")
            )
      it "works as TagsNameExtension" $
        parseFile TagsNameExtension " tag1,tag2,tag3 FileName.ext"
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag1", Present), ("tag2", Present), ("tag3", Present)]
                )
                " tag1,tag2,tag3 "
                "FileName"
                (Just ".ext")
            )
      it "works on extensionless files" $
        parseFile NameTagsExtension "FileName tag1,tag2,tag3 "
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag1", Present), ("tag2", Present), ("tag3", Present)]
                )
                " tag1,tag2,tag3 "
                "FileName"
                Nothing
            )
      it "is greedy in tags (over extensions)" $
        parseFile NameTagsExtension "FileName tag.1,tag.2,tag.3 "
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag.1", Present), ("tag.2", Present), ("tag.3", Present)]
                )
                " tag.1,tag.2,tag.3 "
                "FileName"
                Nothing
            )
      it "still parses an extension when necessary" $
        parseFile NameTagsExtension "FileName tag.1,tag.2,tag .3 "
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag.1", Present), ("tag.2", Present), ("tag", Present)]
                )
                " tag.1,tag.2,tag "
                "FileName"
                (Just ".3 ")
            )
      it "is greedy in extensions (over filename)" $
        parseFile TagsNameExtension " tag.1,tag.2,tag.3 file.name.tar.gz"
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag.1", Present), ("tag.2", Present), ("tag.3", Present)]
                )
                " tag.1,tag.2,tag.3 "
                "file"
                (Just ".name.tar.gz")
            )
      it "is greedy in extensions (over filename), but will still make a non-empty file name when necessary" $
        parseFile TagsNameExtension " tag.1,tag.2,tag.3 .file.name.tar.gz"
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag.1", Present), ("tag.2", Present), ("tag.3", Present)]
                )
                " tag.1,tag.2,tag.3 "
                ".file"
                (Just ".name.tar.gz")
            )
      it "strips directory information correctly" $
        parseFile NameTagsExtension "./directory/FileName tag1,tag2,tag3 .ext"
          `shouldBe` Just
            ( FileInfo
                ( Tags $
                    M.fromList
                      [("tag1", Present), ("tag2", Present), ("tag3", Present)]
                )
                " tag1,tag2,tag3 "
                "FileName"
                (Just ".ext")
            )
      it "correctly handles a file name containing a tag separator as TagsNameExt when trailing sep is Required" $
        parseFileNoWS TagsNameExtension Required Nothing "[tag ] "
          `shouldBe` Just
            ( FileInfo
                (Tags $ M.fromList [("tag", Present)])
                "[tag ]"
                " "
                Nothing
            )
      it "correctly handles a file name containing a tag separator as TagsNameExt when trailing sep is Optional" $
        parseFileNoWS TagsNameExtension Optional Nothing "[tag ] "
          `shouldBe` Just
            ( FileInfo
                (Tags $ M.fromList [("tag", Present)])
                "[tag ]"
                " "
                Nothing
            )
      it "correctly handles the last tag being a tagged value" $
        parseFileNoWS
          NameTagsExtension
          Forbidden
          (Just $ TaggedValueFormat TagBeforeValue Nothing (Keyword "=") Nothing True)
          "file[tag1=val1 tag2=val2]"
          `shouldBe` Just
            ( FileInfo
                (Tags $ M.fromList [("tag1", Value "val1"), ("tag2", Value "val2")])
                "[tag1=val1 tag2=val2]"
                "file"
                Nothing
            )
