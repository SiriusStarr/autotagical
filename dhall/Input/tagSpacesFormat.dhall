{-|
# Input.tagSpacesFormat

## Format

`Input.tagSpacesFormat` is an input format that matches the tags written by
[TagSpaces](https://github.com/tagspaces/tagspaces).

## Behavior

This input format expects the following:

* Files are tagged in the order `File Name -> Tags -> File Extension`.
* The opening separator for tags is `[`.
* Tags are separated by whitespace.
* The closing separator for tags is `]`.
* Leading tag separators are optional, e.g. `[ tag1 tag2]` and `[tag1 tag2]` are
  both valid.
* Trailing tag separators are optional, e.g. `[tag1 tag2 ]` and `[tag1 tag2]`
  are both valid.
* Tagged values are forbidden, i.e. all tags will be interpreted as purely
  literal tags.

To override any of this behavior, use `Input.With`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.tagSpacesFormat
```

-}

let InputFormat =
        ../Internal/InputFormat/Type sha256:33b17ef96b10f2f3395cb0138d822bb04bbdc89bf57837a9b4de4e60a94a203f
      ? ../Internal/InputFormat/Type

let Order =
        ../Internal/InputFormat/Order sha256:77ea7cca209ab0ef9afd003a580efa8aba29cf572cbe095698679fb25a96d24c
      ? ../Internal/InputFormat/Order

let Separator =
        ../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../Internal/InputFormat/Separator

let SeparatorRequirement =
        ../Internal/InputFormat/SeparatorRequirement sha256:047b627a9979b467750a376b4f61681cd1071d132c71007fcd2f7a00df993a17
      ? ../Internal/InputFormat/SeparatorRequirement

let TaggedValueFormat =
        ../Internal/InputFormat/TaggedValueFormat sha256:62b80103bf88b5ca5f35005b578267a5679fdbdcfb0b77229c3651b882961551
      ? ../Internal/InputFormat/TaggedValueFormat

in    { tagOrder = Order.NameTagsExtension
      , openingSeparator = Separator.Keyword "["
      , leadingTagSeparator = SeparatorRequirement.Optional
      , tagSeparator = Separator.Whitespace
      , taggedValueFormat = None TaggedValueFormat
      , trailingTagSeparator = SeparatorRequirement.Optional
      , closingSeparator = Separator.Keyword "]"
      }
    : InputFormat
