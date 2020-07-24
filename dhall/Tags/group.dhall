{-|
# Tags.group

## Function

Given a list of tags, create a tag group for use with the `hasGroup` predicate.
Groups may also be imported using the functions found in `Tags.ImportGroup`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Tags.group
      [ Autotagical.Tags.tag "tag1", Autotagical.Tags.tag "tag2" ]
```

-}

let Tag =
        ../Internal/Tag/Type sha256:3791aa7a52b92db3877472c57cec53ca856858bbad81156ef1bf29e9aeb2baee
      ? ../Internal/Tag/Type

let TagGroup =
        ../Internal/TagGroup/Type sha256:53d772dbc0ab2e509aa6425d923bc93e4fd75854cfdeb9245c37017f6c08bf29
      ? ../Internal/TagGroup/Type

let group
    : List Tag → TagGroup
    = λ(tags : List Tag) → TagGroup.AutotagicalGroup tags

in  group
