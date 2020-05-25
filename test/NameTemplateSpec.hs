{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NameTemplateSpec
  ( spec,
  )
where

import Arbitrary ()
import qualified Data.Either.Validation as V
import qualified Data.Map as M
import qualified Data.Text as T
import Dhall (Decoder, autoWith, defaultInputNormalizer)
import GHC.Exts (IsList (..))
import NameTemplate
  ( Case (..),
    FileName (..),
    FileNameComponent (..),
    FolderName (..),
    FolderNameComponent (..),
    Interpret (..),
    LatestYear (..),
    MonthFormat (..),
    NameTemplateError (..),
    YearFormat (..),
    translateFileComponent,
    translateFileComponents,
    translateFileName,
    translateFolderComponent,
    translateFolderComponents,
    translateFolderName,
  )
import Numeric.Natural (Natural)
import Parser (FileInfo (..))
import Predicate (Predicate (..), checkPredicate)
import SafeType (safeText)
import Tag (Tag (..), TagValue (..), Tags (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Utility (importFails, importSucceeds, toSafeText)

-- | For testing interpreting number as year
genOutOfRangeNatural :: Gen Natural
genOutOfRangeNatural =
  arbitrary
    `suchThat` ( \n ->
                   ( (n :: Natural) > 99
                       && (n :: Natural) < 1000
                   )
                     || n
                     > 9999
               )

-- | Convert a `FolderNameComponent` to a `FileNameComponent` for testing
convertComponent :: FolderNameComponent -> FileNameComponent
convertComponent (FolderFormatAs c f) = FileFormatAs (convertComponent c) f
convertComponent (FolderIfThenElse p ts fs) =
  FileIfThenElse p (convertComponent <$> ts) (convertComponent <$> fs)
convertComponent (FolderInterpret c i) = FileInterpret (convertComponent c) i
convertComponent FolderOriginalName = FileOriginalName
convertComponent (FolderTagValue t) = FileTagValue t
convertComponent (FolderTextLiteral t) = FileTextLiteral t

-- | Check if two accumulated NameTemplateErrors are equivalent
sameError :: [NameTemplateError] -> [NameTemplateError] -> Bool
sameError e1s e2s = and $ zipWith same e1s e2s
  where
    same TagValueWithoutTag {} TagValueWithoutTag {} = True
    same TagValueWithoutTag {} _ = False
    same NullNameTemplate {} NullNameTemplate {} = True
    same NullNameTemplate {} _ = False
    same InterpretNumberOnNonNumber {} InterpretNumberOnNonNumber {} = True
    same InterpretNumberOnNonNumber {} _ = False
    same NumberAsMonthOutOfRange {} NumberAsMonthOutOfRange {} = True
    same NumberAsMonthOutOfRange {} _ = False
    same NumberAsYearOutOfRange {} NumberAsYearOutOfRange {} = True
    same NumberAsYearOutOfRange {} _ = False

spec :: Spec
spec = parallel $ do
  context "FromDhall instances" $ do
    describe "Case"
      $ it "imports all correctly"
      $ let d = autoWith defaultInputNormalizer :: Decoder [Case]
         in importSucceeds d [AsCamelCase, AsPascalCase, AsSnakeCase, AsSpinalCase, AsTitleCase, AsTrainCase] "case.dhall"
    describe "MonthFormat"
      $ it "imports all correctly"
      $ let d = autoWith defaultInputNormalizer :: Decoder [MonthFormat]
         in importSucceeds d [MonthName, ThreeLetterMonth, TwoDigitMonth, OneDigitMonth] "monthFormat.dhall"
    describe "YearFormat"
      $ it "imports all correctly"
      $ let d = autoWith defaultInputNormalizer :: Decoder [YearFormat]
         in importSucceeds d [FourDigitYear, TwoDigitYear] "yearFormat.dhall"
    describe "LatestYear" $ do
      let d = autoWith defaultInputNormalizer :: Decoder LatestYear
      it "imports correctly" $
        importSucceeds d (LatestYear 2021) "latestYear.dhall"
      it "fails when < 99" $
        importFails d "latestYear-tooSmall.dhall"
    describe "Interpret" $ do
      let d = autoWith defaultInputNormalizer :: Decoder Interpret
      it "imports NumberAsOrdinal" $
        importSucceeds d NumberAsOrdinal "interpret-numberAsOrdinal.dhall"
      it "imports NumberAsMonth" $
        importSucceeds d (NumberAsMonth MonthName) "interpret-numberAsMonth.dhall"
      it "imports NumberAsYear" $
        importSucceeds d (NumberAsYear (LatestYear 2021) FourDigitYear) "interpret-numberAsYear.dhall"
      it "fails with invalid latest year" $
        importFails d "interpret-numberAsYear-invalidLatestYear.dhall"
    describe "FolderNameComponent" $ do
      let d = autoWith defaultInputNormalizer :: Decoder FolderNameComponent
      it "imports Format.asCamelCase" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsCamelCase) "folderNameComponent-format-asCamelCase.dhall"
      it "imports Format.AsPascalCase" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsPascalCase) "folderNameComponent-format-asPascalCase.dhall"
      it "imports Format.as_snake_case" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsSnakeCase) "folderNameComponent-format-asSnakeCase.dhall"
      it "imports Format.as-spinal-case" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsSpinalCase) "folderNameComponent-format-asSpinalCase.dhall"
      it "imports Format.`As Title Case`" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsTitleCase) "folderNameComponent-format-asTitleCase.dhall"
      it "imports Format.As-Train-Case" $
        importSucceeds d (FolderFormatAs (FolderTextLiteral "test") AsTrainCase) "folderNameComponent-format-asTrainCase.dhall"
      it "imports ifThenElse" $
        importSucceeds
          d
          (FolderIfThenElse Always [FolderTextLiteral "test"] [FolderOriginalName])
          "folderNameComponent-ifThenElse.dhall"
      it "imports Interpret.numberAsMonth" $
        importSucceeds
          d
          (FolderInterpret (FolderTextLiteral "01") $ NumberAsMonth ThreeLetterMonth)
          "folderNameComponent-interpret-numberAsMonth.dhall"
      it "imports Interpret.numberAsOrdinal" $
        importSucceeds
          d
          (FolderInterpret (FolderTextLiteral "01") NumberAsOrdinal)
          "folderNameComponent-interpret-numberAsOrdinal.dhall"
      it "imports Interpret.numberAsYear" $
        importSucceeds
          d
          (FolderInterpret (FolderTextLiteral "01") $ NumberAsYear (LatestYear 2021) FourDigitYear)
          "folderNameComponent-interpret-numberAsYear.dhall"
      it "imports when" $
        importSucceeds d (FolderIfThenElse Always [FolderOriginalName] []) "folderNameComponent-when.dhall"
      it "imports unless" $
        importSucceeds d (FolderIfThenElse Always [] [FolderOriginalName]) "folderNameComponent-unless.dhall"
      it "imports originalName" $
        importSucceeds d FolderOriginalName "folderNameComponent-originalName.dhall"
      it "imports tagValue" $
        importSucceeds d (FolderTagValue "test") "folderNameComponent-tagValue.dhall"
      it "imports text" $
        importSucceeds d (FolderTextLiteral "literal") "folderNameComponent-textLiteral.dhall"
      it "fails if text is empty" $
        importFails d "folderNameComponent-textEmpty.dhall"
      it "fails if text contains a slash" $
        importFails d "folderNameComponent-textSlash.dhall"
      it "fails if both IfThenElse components are empty" $
        importFails d "folderNameComponent-ifThenElseBothEmpty.dhall"
      it "fails on deeply nested invalid components" $
        importFails d "folderNameComponent-nestedInvalid.dhall"
    describe "FolderName" $ do
      let d = autoWith defaultInputNormalizer :: Decoder FolderName
      it "imports successfully with a separator" $
        importSucceeds
          d
          ( FolderName
              [FolderTextLiteral "test", FolderOriginalName]
              (Just ",")
          )
          "folderName-separator.dhall"
      it "imports successfully without a separator" $
        importSucceeds
          d
          ( FolderName
              [FolderTextLiteral "test", FolderOriginalName]
              Nothing
          )
          "folderName-withoutSeparator.dhall"
      it "fails with an empty list" $
        importFails d "folderName-empty.dhall"
      it "fails with an empty separator" $
        importFails d "folderName-emptySeparator.dhall"
      it "fails with a slash in the separator" $
        importFails d "folderName-slash.dhall"
    describe "FileNameComponent" $ do
      let d = autoWith defaultInputNormalizer :: Decoder FileNameComponent
      it "imports duplicateNumber" $
        importSucceeds d FileDuplicateNumber "fileNameComponent-duplicateNumber.dhall"
      it "imports Format.asCamelCase" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsCamelCase) "fileNameComponent-format-asCamelCase.dhall"
      it "imports Format.AsPascalCase" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsPascalCase) "fileNameComponent-format-asPascalCase.dhall"
      it "imports Format.as_snake_case" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsSnakeCase) "fileNameComponent-format-asSnakeCase.dhall"
      it "imports Format.as-spinal-case" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsSpinalCase) "fileNameComponent-format-asSpinalCase.dhall"
      it "imports Format.`As Title Case`" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsTitleCase) "fileNameComponent-format-asTitleCase.dhall"
      it "imports Format.As-Train-Case" $
        importSucceeds d (FileFormatAs (FileTextLiteral "test") AsTrainCase) "fileNameComponent-format-asTrainCase.dhall"
      it "imports ifDuplicate" $
        importSucceeds
          d
          (FileIfDuplicate [FileDuplicateNumber, FileOriginalName])
          "fileNameComponent-ifDuplicate.dhall"
      it "imports ifThenElse" $
        importSucceeds
          d
          (FileIfThenElse Always [FileTextLiteral "test"] [FileOriginalName])
          "fileNameComponent-ifThenElse.dhall"
      it "imports Interpret.numberAsMonth" $
        importSucceeds
          d
          (FileInterpret (FileTextLiteral "01") $ NumberAsMonth ThreeLetterMonth)
          "fileNameComponent-interpret-numberAsMonth.dhall"
      it "imports Interpret.numberAsOrdinal" $
        importSucceeds
          d
          (FileInterpret (FileTextLiteral "01") NumberAsOrdinal)
          "fileNameComponent-interpret-numberAsOrdinal.dhall"
      it "imports Interpret.numberAsYear" $
        importSucceeds
          d
          (FileInterpret (FileTextLiteral "01") $ NumberAsYear (LatestYear 2021) FourDigitYear)
          "fileNameComponent-interpret-numberAsYear.dhall"
      it "imports originalName" $
        importSucceeds d FileOriginalName "fileNameComponent-originalName.dhall"
      it "imports tagValue" $
        importSucceeds d (FileTagValue "test") "fileNameComponent-tagValue.dhall"
      it "imports text" $
        importSucceeds d (FileTextLiteral "literal") "fileNameComponent-textLiteral.dhall"
      it "imports unless" $
        importSucceeds d (FileIfThenElse Always [] [FileOriginalName]) "fileNameComponent-unless.dhall"
      it "imports when" $
        importSucceeds d (FileIfThenElse Always [FileOriginalName] []) "fileNameComponent-when.dhall"
      it "fails if text is empty" $
        importFails d "fileNameComponent-textEmpty.dhall"
      it "fails if text contains a slash" $
        importFails d "fileNameComponent-textSlash.dhall"
      it "fails if both IfThenElse components are empty" $
        importFails d "fileNameComponent-ifThenElseBothEmpty.dhall"
      it "fails if ifDuplicate is empty" $
        importFails d "fileNameComponent-ifDuplicateEmpty.dhall"
      it "fails on deeply nested invalid components" $
        importFails d "fileNameComponent-nestedInvalid.dhall"
    describe "FileName" $ do
      let d = autoWith defaultInputNormalizer :: Decoder FileName
      it "imports successfully with a separator" $
        importSucceeds
          d
          (FileName [FileTextLiteral "test", FileOriginalName] (Just ","))
          "fileName-separator.dhall"
      it "imports successfully without a separator" $
        importSucceeds
          d
          (FileName [FileTextLiteral "test", FileOriginalName] Nothing)
          "fileName-withoutSeparator.dhall"
      it "fails with an empty list" $
        importFails d "fileName-empty.dhall"
      it "fails with an empty separator" $
        importFails d "fileName-emptySeparator.dhall"
      it "fails with a slash in the separator" $
        importFails d "fileName-slash.dhall"
  describe "translateFolderComponent" $ do
    context "when component is Format" $ do
      prop "properly formats camelCase" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "This_isCamelCasing") AsCamelCase)
            `shouldBe` V.Success "thisIsCamelCasing"
      prop "properly formats PascalCase" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "This_isPascal-Casing") AsPascalCase)
            `shouldBe` V.Success
              "ThisIsPascalCasing"
      prop "properly formats snake_case" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "This_is-Snake_casing") AsSnakeCase)
            `shouldBe` V.Success "this_is_snake_casing"
      prop "properly formats spinal-case" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "This_is_SpinalTap") AsSpinalCase)
            `shouldBe` V.Success "this-is-spinal-tap"
      prop "properly formats Title Case" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "this_IsTitle_Casing") AsTitleCase)
            `shouldBe` V.Success "This Is Title Casing"
      prop "properly formats Train-Case" $
        \f sep ->
          translateFolderComponent
            f
            sep
            (FolderFormatAs (FolderTextLiteral "this_Is-train_Casing") AsTrainCase)
            `shouldBe` V.Success "This-Is-Train-Casing"
    context "when component is ifThenElse" $ do
      prop "is always equal to the true components if predicate is true" $
        \p ts fs f sep ->
          checkPredicate (fileTags f) p
            ==> translateFolderComponent f sep (FolderIfThenElse p ts fs)
              `shouldBe` translateFolderComponents f sep ts
      prop "is always equal to the false components if predicate is false" $
        \p ts fs f sep ->
          not (checkPredicate (fileTags f) p)
            ==> translateFolderComponent f sep (FolderIfThenElse p ts fs)
              `shouldBe` translateFolderComponents f sep fs
    context "when component is Interpret" $ do
      prop "fails appropriately on non-numbers" $
        \i f sep ->
          translateFolderComponent
            f
            sep
            (FolderInterpret (FolderTextLiteral "sd") i)
            `shouldSatisfy` ( \case
                                V.Failure [InterpretNumberOnNonNumber {}] -> True
                                _ -> False
                            )
      context "when numberAsMonth" $ do
        let numbers = toSafeText . T.pack . show <$> ([1 .. 12] :: [Natural])
        prop "properly outputs all month names" $
          \f sep ->
            traverse
              ( \t ->
                  translateFolderComponent
                    f
                    sep
                    ( FolderInterpret
                        (FolderTextLiteral t)
                        (NumberAsMonth MonthName)
                    )
              )
              numbers
              `shouldBe` V.Success ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        prop "properly outputs all three letter month names" $
          \f sep ->
            traverse
              ( \t ->
                  translateFolderComponent
                    f
                    sep
                    ( FolderInterpret
                        (FolderTextLiteral t)
                        (NumberAsMonth ThreeLetterMonth)
                    )
              )
              numbers
              `shouldBe` V.Success ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        prop "properly outputs 2 digit months" $
          \f sep ->
            traverse
              ( \t ->
                  translateFolderComponent
                    f
                    sep
                    ( FolderInterpret
                        (FolderTextLiteral t)
                        (NumberAsMonth TwoDigitMonth)
                    )
              )
              numbers
              `shouldBe` V.Success ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
        prop "properly outputs 1 digit months" $
          \f sep ->
            traverse
              ( \t ->
                  translateFolderComponent
                    f
                    sep
                    ( FolderInterpret
                        (FolderTextLiteral t)
                        (NumberAsMonth OneDigitMonth)
                    )
              )
              numbers
              `shouldBe` V.Success (safeText <$> numbers)
        prop "fails appropriately on numbers out of range" $
          \format f sep n ->
            (n :: Natural) == 0 || n > 12
              ==> translateFolderComponent
                f
                sep
                ( FolderInterpret
                    (FolderTextLiteral . toSafeText . T.pack . show $ n)
                    (NumberAsMonth format)
                )
              `shouldSatisfy` ( \case
                                  V.Failure [NumberAsMonthOutOfRange {}] -> True
                                  _ -> False
                              )
      context "when numberAsOrdinal"
        $ prop "properly interprets numbers"
        $ \f sep ->
          translateFolderComponent
            f
            sep
            ( FolderInterpret
                (FolderTextLiteral "5")
                NumberAsOrdinal
            )
            `shouldBe` V.Success "5th"
      context "when numberAsYear" $ do
        prop "properly outputs 4 digit years given 4 digit years" $
          \f sep y ->
            translateFolderComponent
              f
              sep
              ( FolderInterpret
                  (FolderTextLiteral "1952")
                  (NumberAsYear y FourDigitYear)
              )
              `shouldBe` V.Success "1952"
        prop "properly outputs 2 digit years given 4 digit years" $
          \f sep y ->
            translateFolderComponent
              f
              sep
              ( FolderInterpret
                  (FolderTextLiteral "1952")
                  (NumberAsYear y TwoDigitYear)
              )
              `shouldBe` V.Success "52"
        prop "properly interprets years before latest year" $
          \f sep ->
            translateFolderComponent
              f
              sep
              ( FolderInterpret
                  (FolderTextLiteral "19")
                  (NumberAsYear (LatestYear 2021) FourDigitYear)
              )
              `shouldBe` V.Success "2019"
        prop "properly interprets years after latest year" $
          \f sep ->
            translateFolderComponent
              f
              sep
              ( FolderInterpret
                  (FolderTextLiteral "51")
                  (NumberAsYear (LatestYear 2021) FourDigitYear)
              )
              `shouldBe` V.Success "1951"
        prop "properly interprets years equal to latest year" $
          \f sep ->
            translateFolderComponent
              f
              sep
              ( FolderInterpret
                  (FolderTextLiteral "21")
                  (NumberAsYear (LatestYear 2021) FourDigitYear)
              )
              `shouldBe` V.Success "2021"
        prop "fails on years in 100-1000 or >9999" $
          forAll
            genOutOfRangeNatural
            ( \n f sep y yf ->
                translateFolderComponent
                  f
                  sep
                  ( FolderInterpret
                      (FolderTextLiteral . toSafeText . T.pack . show $ n)
                      (NumberAsYear y yf)
                  )
                  `shouldSatisfy` \case
                    V.Failure [NumberAsYearOutOfRange {}] -> True
                    _ -> False
            )
    context "when component is originalName"
      $ prop "is always equal to the original file name"
      $ \f sep ->
        translateFolderComponent f sep FolderOriginalName
          `shouldBe` (V.Success . safeText . originalName $ f)
    context "when component is tagValue" $ do
      prop "is always equal to the value of the tag if the tag exists and is a tagged value" $
        \t v ts raw name ext sep ->
          translateFolderComponent
            (FileInfo (Tags $ M.insert t (Value v) ts) raw name ext)
            sep
            (FolderTagValue t)
            `shouldBe` V.Success (safeText v)
      prop "is always equal to the name of the tag if the tag exists and is merely Present" $
        \t ts raw name ext sep ->
          translateFolderComponent
            (FileInfo (Tags $ M.insert t Present ts) raw name ext)
            sep
            (FolderTagValue t)
            `shouldBe` (V.Success . safeText . Tag.tag $ t)
      prop "errors out if the tag isn't found" $
        \t ts raw name ext sep ->
          translateFolderComponent
            (FileInfo (Tags $ M.delete t ts) raw name ext)
            sep
            (FolderTagValue t)
            `shouldBe` V.Failure [TagValueWithoutTag t]
    context "when component is text"
      $ prop "is just the text, literally"
      $ \t f sep ->
        translateFolderComponent f sep (FolderTextLiteral t)
          `shouldBe` V.Success (safeText t)
  describe "translateFolderComponents" $ do
    prop "is always equivalent to translateFolderComponent for a single component" $
      \c f sep ->
        translateFolderComponents f sep [c]
          `shouldBe` translateFolderComponent f sep c
    prop "is always empty for no components" $
      \f sep ->
        translateFolderComponents f sep []
          `shouldBe` V.Success ""
    prop "does not introduce extra separators for empty components" $
      \f ->
        translateFolderComponents f " " [FolderTextLiteral "a", FolderIfThenElse Always [] [], FolderTextLiteral "b", FolderIfThenElse Always [] []]
          `shouldBe` V.Success "a b"
    prop "is always equivalent to non-empty translateFolderComponents separated by sep for multiple components" $
      \cs f sep ->
        translateFolderComponents f sep cs
          `shouldBe` ( T.intercalate sep . filter (not . T.null)
                         <$> traverse (translateFolderComponent f sep) cs
                     )
  describe "translateFolderName" $ do
    prop "is always equivalent to translateFolderComponents if not null" $
      \f cs sep ->
        let fromCs = translateFolderComponents f (maybe "" safeText sep) (toList cs)
         in fromCs /= V.Success ""
              ==> translateFolderName f (FolderName cs sep) `shouldBe` fromCs
    prop "is never null" $
      \f n ->
        translateFolderName f n `shouldNotBe` V.Success ""
    prop "always errors out for empty templates" $
      \f ->
        translateFolderName
          f
          ( FolderName
              [ FolderIfThenElse Always [] [FolderTextLiteral "a"],
                FolderIfThenElse (Not Always) [FolderTextLiteral "b"] []
              ]
              Nothing
          )
          `shouldBe` V.Failure
            [ NullNameTemplate
                ( T.pack $
                    show
                      ( FolderName
                          [ FolderIfThenElse Always [] [FolderTextLiteral "a"],
                            FolderIfThenElse (Not Always) [FolderTextLiteral "b"] []
                          ]
                          Nothing
                      )
                )
            ]
  describe "translateFileComponent" $ do
    context "when component is duplicateNumber" $ do
      prop "is always null for non-duplicates" $
        \f sep ->
          translateFileComponent f Nothing sep FileDuplicateNumber
            `shouldBe` V.Success ""
      prop "is simply the duplicate number for duplicates" $
        \f n sep ->
          translateFileComponent f (Just n) sep FileDuplicateNumber
            `shouldBe` (V.Success . T.pack . show $ n)
    context "when component is ifDuplicate" $ do
      prop "is always equal to the components if duplicate" $
        \f n sep cs ->
          translateFileComponent f (Just n) sep (FileIfDuplicate cs)
            `shouldBe` translateFileComponents f (Just n) sep (toList cs)
      prop "is always null if not a duplicate" $
        \f sep cs ->
          translateFileComponent f Nothing sep (FileIfDuplicate cs)
            `shouldBe` V.Success ""
    prop "when component is anything else, is equal to folder component" $
      \f d sep c ->
        ( translateFolderComponent f sep c,
          translateFileComponent f d sep (convertComponent c)
        )
          `shouldSatisfy` ( \case
                              (V.Success t1, V.Success t2) -> t1 == t2
                              (V.Failure e1, V.Failure e2) ->
                                sameError e1 e2
                              _ -> False
                          )
  describe "translateFileComponents" $ do
    prop "is always equivalent to translateFileComponent for a single component" $
      \c f d sep ->
        translateFileComponents f d sep [c]
          `shouldBe` translateFileComponent f d sep c
    prop "is always empty for no components" $
      \f d sep ->
        translateFileComponents f d sep []
          `shouldBe` V.Success ""
    prop "does not introduce extra separators for empty components" $
      \f ->
        translateFileComponents
          f
          Nothing
          " "
          [ FileTextLiteral "a",
            FileIfThenElse Always [] [],
            FileTextLiteral "b",
            FileDuplicateNumber,
            FileIfDuplicate [FileTextLiteral "c"]
          ]
          `shouldBe` V.Success "a b"
    prop "is always equivalent to non-empty translateFileComponents separated by sep for multiple components" $
      \cs f d sep ->
        translateFileComponents f d sep cs
          `shouldBe` ( T.intercalate sep . filter (not . T.null)
                         <$> traverse (translateFileComponent f d sep) cs
                     )
  describe "translateFileName" $ do
    prop "is always equivalent to translateFileComponents if not null" $
      \f d cs sep ->
        let fromCs = translateFileComponents f d (maybe "" safeText sep) (toList cs)
         in fromCs /= V.Success ""
              ==> translateFileName f d (FileName cs sep) `shouldBe` fromCs
    prop "is never null" $
      \f d n ->
        translateFileName f d n `shouldNotBe` V.Success ""
    prop "always errors out for empty templates" $
      \f d ->
        translateFileName
          f
          d
          ( FileName
              [ FileIfThenElse Always [] [],
                FileIfThenElse Always [] []
              ]
              Nothing
          )
          `shouldBe` V.Failure
            [ NullNameTemplate
                ( T.pack $
                    show
                      ( FileName
                          [ FileIfThenElse Always [] [],
                            FileIfThenElse Always [] []
                          ]
                          Nothing
                      )
                )
            ]
