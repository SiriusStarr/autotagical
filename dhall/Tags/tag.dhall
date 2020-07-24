{-|
# Tags.tag

## Function

Given text, create a tag (for the purposes of creating a tag group, for the
`hasTag` predicate, etc.).  Note that the text will match the *tag* part of a
tagged value, or the *entirety* of a non-tagged value.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Tags.tag "tag1"
```

-}

let Tag =
        ../Internal/Tag/Type sha256:3791aa7a52b92db3877472c57cec53ca856858bbad81156ef1bf29e9aeb2baee
      ? ../Internal/Tag/Type

let tag
    : Text → Tag
    = λ(tag : Text) → { tag }

in  tag
