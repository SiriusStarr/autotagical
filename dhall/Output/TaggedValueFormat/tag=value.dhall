{-
# Output.TaggedValueFormat.tag=value

## Function

Given a boolean indicating whether pure tags (i.e. non-tagged values) are
allowed, returns a tagged value format that writes the standard `tag=value`
format for tagged values.

Note that since this uses an `=` in the record field, it must be accessed by
surrounding the field with backticks (\`), as shown in Usage below.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Output.TaggedValueFormat.`tag=value` True
```

-}

let TaggedValueFormat =
        ../../Internal/OutputFormat/TaggedValueFormat sha256:695155008931bf451f550cd682d0057920d1498dceac6e133774c6b200c6ba40
      ? ../../Internal/OutputFormat/TaggedValueFormat

let tagsBeforeValues =
        ./tagsBeforeValues.dhall sha256:deb356761e0161f49f51ea06b23db36b4ba8aba986d547edfdf3390d88253458
      ? ./tagsBeforeValues.dhall

let tagsBeforeValues
    : Bool → TaggedValueFormat
    = λ(pureTagsAllowed : Bool) →
        tagsBeforeValues (None Text) "=" (None Text) pureTagsAllowed

in  tagsBeforeValues
