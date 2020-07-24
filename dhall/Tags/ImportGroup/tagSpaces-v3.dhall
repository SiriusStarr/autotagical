{-|
# Tags.ImportGroup.tagSpaces-v3

## Function

Given JSON in Text form and the name of the group to import as Text, create a
tag group for use with the `hasGroup` predicate from TagSpaces JSON.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Tags.ImportGroup.tagSpaces-v3
      ./Path/To/TagSpaces-v3.json as Text
      "GroupName"
```

-}

let TagGroup =
        ../../Internal/TagGroup/Type sha256:53d772dbc0ab2e509aa6425d923bc93e4fd75854cfdeb9245c37017f6c08bf29
      ? ../../Internal/TagGroup/Type

let tagSpaces-v3
    : Text → Text → TagGroup
    = λ(json : Text) →
      λ(groupName : Text) →
        TagGroup.TagSpaces-v3 { json, groupName }

in  tagSpaces-v3
