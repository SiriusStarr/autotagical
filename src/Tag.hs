{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tag
-- Description : Tags and collections of tags
--
-- Individual tags, their tagged values, maps of tags on files, and groups
-- tags for predicates, as well as Dhall instances for them.
module Tag
  ( -- * Tags
    Tag (..),
    TagValue (..),
    Tags (..),

    -- * Tag Groups
    TagGroup (..),
  )
where

import qualified Data.Aeson as Json
import Data.Aeson ((.:), FromJSON)
import Data.Either.Validation (Validation (..))
import Data.List (find, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (IsString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Dhall (Decoder (..), FromDhall, autoWith, extractError)
import qualified Dhall as D
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import SafeType (NonEmptySet, SafeText, makeNonEmptySet, safeText, validateText)

-- * Tags

-- | A tag, as it appears in tag groups or on a file.
newtype Tag = Tag {tag :: SafeText}
  deriving (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromDhall Tag where
  autoWith opts = D.record $ Tag <$> D.field "tag" (autoWith opts)

-- | The value a tag on a file has.  `Present` means it is not a tagged value.
data TagValue
  = Present
  | Value SafeText
  deriving (Eq, Generic)

-- | `TagValue`s are only shown in combination with `Tag`s, so only need to
--   display values, not `Present`.
instance Show TagValue where
  show Present = ""
  show (Value v) = " = " <> show v

-- | The tags on a parsed file, complete with tagged value, if relevant.
newtype Tags = Tags {tagMap :: Map Tag TagValue} deriving (Eq)

instance Show Tags where
  show (Tags ts) =
    "Tags: [ "
      <> intercalate ", " ((\(t, v) -> show t <> show v) <$> M.assocs ts)
      <> " ]"

-- * Tag Groups

-- | A group of tags, for querying by a `Predicate.Predicate`.
newtype TagGroup = TagGroup {groupTags :: NonEmptySet Tag}
  deriving (Eq)
  deriving newtype (Show)

-- | Wrapper for TagSpaces v3 format for `FromJSON` instance.
newtype TagSpaces_v3 = TagSpaces_v3 [TagSpaces_v3_TagGroup]

-- | Aeson `FromJSON` instance for TagSpaces version 3 format.
instance FromJSON TagSpaces_v3 where
  parseJSON = Json.withObject "TagSpaces-v3" $ \o -> do
    tagGroups <- o .: "tagGroups"
    return $ TagSpaces_v3 tagGroups

-- | Wrapper for each tag group in TagSpaces JSON format.
newtype TagSpaces_v3_TagGroup = TagSpaces_v3_TagGroup (Text, [TagSpaces_v3_Tag])

-- | Aeson `FromJSON` instance for TagSpaces version 3 tag group.
instance FromJSON TagSpaces_v3_TagGroup where
  parseJSON = Json.withObject "tagGroup" $ \o -> do
    title <- o .: "title"
    children <- o .: "children"
    return $ TagSpaces_v3_TagGroup (title, children)

-- | Wrapper for each tag in a TagSpaces JSON tag group.
newtype TagSpaces_v3_Tag = TagSpaces_v3_Tag Text

-- | Aeson `FromJSON` instance for TagSpaces version 3 tag.
instance FromJSON TagSpaces_v3_Tag where
  parseJSON = Json.withObject "tag" $ \o -> do
    title <- o .: "title"
    return $ TagSpaces_v3_Tag title

-- | Covert decoded TagSpaces version 3 JSON to a `TagGroup`.
convertTagSpaces_v3 :: Decoder (Either String TagSpaces_v3, SafeText) -> Decoder TagGroup
convertTagSpaces_v3 (Decoder input expect) = Decoder out expect
  where
    out e = case input e of
      Success (Left err, group) ->
        extractError $
          T.concat
            [ "Error loading TagSpaces-v3 format for specified group: ",
              T.pack $ show group,
              "\nThis probably means the JSON is not the specified format.\nThe following error was encountered:\n",
              T.pack err
            ]
      Success (Right (TagSpaces_v3 gs), group) ->
        case find (\(TagSpaces_v3_TagGroup (g, _)) -> g == safeText group) gs of
          Nothing ->
            extractError $
              T.concat
                [ "TagSpaces-v3 format was decoded successfully but did not contain the specified tag group.  The specified group was: ",
                  T.pack $ show group,
                  "\nBut the file only contained the following groups: ",
                  T.intercalate ", " $
                    fmap (\(TagSpaces_v3_TagGroup (t, _)) -> T.pack $ show t) gs
                ]
          Just (TagSpaces_v3_TagGroup (_, [])) ->
            extractError $
              "TagSpaces-v3 format was decoded successfully, but the specified tag group was entirely empty.  The specified group was: "
                <> T.pack (show group)
          Just (TagSpaces_v3_TagGroup (_, tags)) ->
            case traverse (\(TagSpaces_v3_Tag t) -> validateText t) tags of
              Left err ->
                extractError $
                  T.concat
                    [ "TagSpaces-v3 format was decoded successfully and the specified tag group ",
                      T.pack $ show group,
                      " was present, but one or more of the tags were invalid:\n",
                      err
                    ]
              Right ts ->
                Success . TagGroup . makeNonEmptySet . fromList . fmap Tag $ ts
      Failure f -> Failure f

-- | `FromDhall` instance for `TagGroup`, allowing a native Dhall format or
--   TagSpaces JSON.
instance FromDhall TagGroup where
  autoWith opts =
    D.union $
      (TagGroup <$> D.constructor "AutotagicalGroup" (autoWith opts))
        <> D.constructor
          "TagSpaces-v3"
          ( convertTagSpaces_v3 $
              D.record
                ( (\json g -> (Json.eitherDecode . encodeUtf8 $ json, g))
                    <$> D.field "json" D.lazyText
                    <*> D.field "groupName" (D.autoWith opts)
                )
          )
