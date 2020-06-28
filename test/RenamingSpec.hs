{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module RenamingSpec
  ( spec,
  )
where

import Arbitrary ()
import Data.Either (fromRight)
import qualified Data.Either.Validation as V
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import GHC.Exts (IsList (..))
import NameTemplate
  ( FileName (..),
    FileNameComponent (..),
    NameTemplateError (..),
  )
import Parser
  ( FileInfo (..),
    InputFormat (..),
    Order,
    OutputFormat (..),
    OutputTaggedValueFormat (..),
    SeparatorRequirement (..),
    TagValueOrder (..),
    TaggedValueFormat (..),
    fileParser,
    parseFileInfo,
    taggedValueParser,
  )
import Predicate (Predicate (..), checkPredicate)
import Renaming
  ( OutputFormatError (..),
    RenamingError (..),
    RenamingRule (..),
    RenamingSchema (..),
    findRenamingRule,
    nameBySchema,
    sepToText,
    writeFileName,
    writeTag,
  )
import SafeType (safeText)
import Tag (Tag (..), TagValue (..), Tags (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec (eof, parseMaybe)
import Utility
  ( convertTVFormat,
    importFails,
    importSucceeds,
    toSafeText,
    validateTaggedValue,
  )

-- | Convert an `InputFormat` to the equivalent `OutputFormat`
inToOutFormat :: InputFormat -> OutputFormat
inToOutFormat (InputFormat ord preSep leadSep tagSep tv trailSep postSep) =
  OutputFormat ord (toSafeText . sepToText $ preSep) (sepReqToBool leadSep) (toSafeText . sepToText $ tagSep) (convertTVFormat <$> tv) (sepReqToBool trailSep) (toSafeText . sepToText $ postSep)
  where
    sepReqToBool Forbidden = False
    sepReqToBool Required = True
    sepReqToBool Optional = False

spec :: Spec
spec = parallel $ do
  context "FromDhall instances" $ do
    describe "RenamingRule" $ do
      let d = autoWith defaultInputNormalizer :: Decoder RenamingRule
      it "imports successfully" $
        importSucceeds
          d
          ( RenamingRule
              ( FileName
                  [ FileTextLiteral "test",
                    FileOriginalName,
                    FileDuplicateNumber
                  ]
                  Nothing
              )
              Always
          )
          "renamingRule.dhall"
    describe "RenamingSchema" $ do
      let d = autoWith defaultInputNormalizer :: Decoder RenamingSchema
      it "imports successfully" $
        importSucceeds
          d
          ( RenamingSchema
              [ RenamingRule
                  ( FileName
                      [ FileTextLiteral "test1",
                        FileOriginalName,
                        FileDuplicateNumber
                      ]
                      Nothing
                  )
                  Always,
                RenamingRule
                  (FileName [FileTextLiteral "test2", FileOriginalName] Nothing)
                  (Not Always)
              ]
          )
          "renamingSchema.dhall"
      it "fails when empty" $
        importFails d "renamingSchema-empty.dhall"
  describe "findRenamingRule" $ do
    prop "always returns the first valid rule" $
      \f cs rs ->
        findRenamingRule f (RenamingSchema . fromList $ RenamingRule cs Always : rs)
          `shouldBe` Just (RenamingRule cs Always)
    prop "never returns an invalid rule" $
      \f rs ->
        (predicate :: RenamingRule -> Predicate) <$> findRenamingRule f rs
          `shouldSatisfy` (checkPredicate (fileTags f) . fromMaybe Always)
  describe "nameBySchema"
    $ prop "errors on a null format string"
    $ \f n cs ->
      let fName = FileName [FileIfThenElse (Not Always) cs []] Nothing
       in nameBySchema (RenamingSchema [RenamingRule fName Always]) n f
            `shouldBe` Left
              (NameTemplateErrors f [NullNameTemplate (T.pack $ show fName)])
  describe "writeTag" $ do
    prop "can always be read back in by a tagged value parser" $
      \inFormat t v ->
        let writtenTag = fromRight "" . V.validationToEither $ writeTag (convertTVFormat <$> inFormat) (t, v)
         in validateTaggedValue inFormat t v && writtenTag /= ""
              ==> parseMaybe (taggedValueParser inFormat eof eof <* eof) writtenTag
                `shouldBe` Just (t, v)
    prop "fails on tagged values when forbidden" $
      \t v ->
        writeTag Nothing (Tag t, Value v)
          `shouldBe` V.Failure [TaggedValueWhenOutputForbidsThem (safeText t) (safeText v)]
    prop "fails on pure tags when forbidden" $
      \f t ->
        writeTag
          (Just $ (f :: OutputTaggedValueFormat) {pureTagsAllowed = False})
          (Tag t, Present)
          `shouldBe` V.Failure [NonTaggedValueWhenOutputForbidsThem (safeText t)]
  describe "writeFileName" $ do
    prop "detects issues with combined tags+separators and errors correctly" $
      \inFormat f tags newName outFormat tvF ->
        writeFileName
          inFormat
          ( Just $
              (outFormat :: OutputFormat)
                { tagSeparator = ">>",
                  trailingTagSeparator = True,
                  taggedValueFormat = Just $ (tvF :: OutputTaggedValueFormat) {tagValueSeparator = "<>"}
                }
          )
          (f {fileTags = Tags $ M.insert "BadTag<" Present tags})
          (safeText <$> newName)
          `shouldSatisfy` \case
            Left _ -> True
            Right _ -> False
    prop "detects issues with tag+separators creating early separators" $
      \inFormat f tags newName outFormat tvF ->
        writeFileName
          inFormat
          ( Just $
              (outFormat :: OutputFormat)
                { taggedValueFormat = Just $ (tvF :: OutputTaggedValueFormat) {tagValueSeparator = "aaaa", order = TagBeforeValue}
                }
          )
          (f {fileTags = Tags $ M.insert "BadTaga" (Value "v") tags})
          (safeText <$> newName)
          `shouldSatisfy` \case
            Left _ -> True
            Right _ -> False
    prop "correctly writes file names parseable by the same input format" $ \inFormat f newName ->
      -- Use SafeText for newName, so it isn't null/etc.
      -- Filter out untagged values or tagged values from random tags so we
      -- aren't just discarding a bunch of cases for the reason of having
      -- tagged values or pure tags when they're not allowed by the format.
      let useTags = case (taggedValueFormat :: InputFormat -> Maybe TaggedValueFormat) inFormat of
            Nothing -> Tags $ Present <$ tagMap (fileTags f)
            Just (TaggedValueFormat _ _ _ _ True) -> fileTags f
            Just (TaggedValueFormat _ _ _ _ False) ->
              Tags . M.filter (Present /=) . tagMap . fileTags $ f
          writeName =
            T.unpack . fromRight "" $
              writeFileName inFormat (Just $ inToOutFormat inFormat) f {fileTags = useTags} (safeText <$> newName)
          parser = fileParser inFormat
          ord = (tagOrder :: InputFormat -> Order) inFormat
       in useTags /= Tags M.empty && writeName /= ""
            ==> parseFileInfo ord parser writeName
              `shouldSatisfy` ( \case
                                  Just (FileInfo writeTags _ outName writeExt) ->
                                    writeTags == useTags
                                      && ( ( outName
                                               == ( case newName of
                                                      Nothing -> originalName f
                                                      Just n -> n
                                                  )
                                               && writeExt
                                               == extension f
                                           )
                                             || (
                                                  -- Tolerate overzealous parsing of extensions, since this is expected
                                                  safeText
                                                    ( case newName of
                                                        Nothing -> originalName f
                                                        Just n -> n
                                                    )
                                                    <> maybe "" safeText (extension f)
                                                    == ( safeText outName
                                                           <> maybe "" safeText writeExt
                                                       )
                                                )
                                         )
                                  _ -> False
                              )
