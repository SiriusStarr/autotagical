{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Utility
  ( makePatterns,
    importSucceeds,
    toSafeText,
    importFails,
    convertTVFormat,
    validateTaggedValue,
  )
where

import Data.Either (isRight)
import Data.Either.Validation (validationToEither)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Dhall (Decoder)
import qualified Dhall as D
import Dhall.Src (Src)
import FileHandler (GlobPatterns (..))
import GHC.Exts (IsList (..))
import Parser (OutputTaggedValueFormat (..), TaggedValueFormat (..))
import Renaming (sepToText, tagsSepCollision, trailingSepCollision)
import SafeType (SafeText, validateText)
import System.FilePath (joinPath)
import System.FilePath.Glob (CompOptions (..))
import qualified System.FilePath.Glob as G
import Tag (Tag (..), TagValue (..), Tags (..))
import Test.Hspec

-- | Unsafely turn `Text` into `SafeText` or throw an exception.
toSafeText :: Text -> SafeText
toSafeText (validateText -> Right t) = t
toSafeText _ = error "Tried to turn unsafe text into SafeText in test suite!  Please report this as a bug."

-- | Turn a list of `Text` into glob patterns
makePatterns :: Bool -> [Text] -> GlobPatterns
makePatterns errorRecovery =
  GlobPatterns . fromList
    . fmap
      ( \p ->
          (p, G.compileWith (G.compDefault {errorRecovery}) $ T.unpack p)
      )

-- | Confirm that a specified Dhall file decodes to the expected Haskell value.
importSucceeds :: (Eq a, Show a) => Decoder a -> a -> FilePath -> Expectation
importSucceeds d expect f =
  D.inputFile d (joinPath [".", "test", "dhall", "success", f])
    `shouldReturn` expect

-- | Confirm that a specified Dhall file fails to decode with an extract error.
importFails :: Decoder a -> FilePath -> Expectation
importFails d f =
  D.inputFile d (joinPath [".", "test", "dhall", "failure", f])
    `shouldThrow` dhallExtractError
  where
    dhallExtractError :: D.ExtractErrors Src Void -> Bool
    dhallExtractError = const True

-- | Convert a `TaggedValueFormat` to an `OutputTaggedValueFormat`
convertTVFormat :: TaggedValueFormat -> OutputTaggedValueFormat
convertTVFormat (TaggedValueFormat ord preSep tvSep postSep pureTags) =
  OutputTaggedValueFormat
    ord
    (toSafeText . sepToText <$> preSep)
    (toSafeText . sepToText $ tvSep)
    (toSafeText . sepToText <$> postSep)
    pureTags

-- | Confirm a tagged value isn't incompatible with a tagged value format
validateTaggedValue :: Maybe TaggedValueFormat -> Tag -> TagValue -> Bool
validateTaggedValue Nothing _ (Value _) = False
validateTaggedValue Nothing _ _ = True
validateTaggedValue (Just (TaggedValueFormat _ _ _ _ False)) _ Present = False
validateTaggedValue (Just tvF@(TaggedValueFormat _ _ tvSep _ _)) t v =
  ( isRight . validationToEither $
      trailingSepCollision
        (sepToText tvSep)
        (Just $ convertTVFormat tvF)
        (sepToText tvSep)
        (t, v)
  )
    && ( isRight . validationToEither
           . traverse_ (tagsSepCollision (Tags $ M.singleton t v))
           . ( \(OutputTaggedValueFormat _ pre tv post _) ->
                 maybe [] (\sep -> [("pre-tagged value", sep)]) pre
                   ++ ("tagged value", tv)
                     : maybe [] (\sep -> [("post-tagged value", sep)]) post
             )
           $ convertTVFormat tvF
       )
