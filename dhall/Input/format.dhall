{-
# Input.format

## Function

Given an opening `Separator` for tags, a `Separator` between tags, and a closing
`Separator` for tags, create an input format that recognizes files tagged with
that format.

## Default Behavior

By default, input formats created with this function expect the following:

* Files are tagged in the order `File Name -> Tags -> File Extension`.
* Leading tag separators are optional, e.g. `[,tag1,tag2]` and `[tag1,tag2]` are
  both valid, if the tag separator is `,`.
* Trailing tag separators are optional, e.g. `[tag1,tag2,]` and `[tag1,tag2]`
  are both valid, if the tag separator is `,`.
* Tagged values are forbidden, i.e. all tags will be interpreted as purely
  literal tags.

To override any of this behavior, use `Input.With`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.format
      (Autotagical.Input.Separator.keyword "[")
      Autotagical.Input.Separator.whitespace
      (Autotagical.Input.Separator.keyword "]")
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

let format
    : Separator → Separator → Separator → InputFormat
    = λ(openingSeparator : Separator) →
      λ(tagSeparator : Separator) →
      λ(closingSeparator : Separator) →
        { tagOrder = Order.NameTagsExtension
        , openingSeparator
        , leadingTagSeparator = SeparatorRequirement.Optional
        , tagSeparator
        , taggedValueFormat = None TaggedValueFormat
        , trailingTagSeparator = SeparatorRequirement.Optional
        , closingSeparator
        }

in  format
