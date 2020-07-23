{-
# Output.tagSpacesFormat

## Format

`Output.tagSpacesFormat` is an output format that matches the tags recognized by
[TagSpaces](https://github.com/tagspaces/tagspaces).

## Behavior

This output format defaults to the following:

* Files will be tagged in the order `File Name -> Tags -> File Extension`.
* The opening separator for tags is `[`.
* Tags are separated by a single space character.
* The closing separator for tags is `]`.
* Leading tag separators will not be written.
* Trailing tag separators will not be written.
* Tagged values are forbidden.

To override any of this behavior, use `Output.With`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Output.tagSpacesFormat
```

-}

let Order =
        ../Internal/InputFormat/Order sha256:77ea7cca209ab0ef9afd003a580efa8aba29cf572cbe095698679fb25a96d24c
      ? ../Internal/InputFormat/Order

let OutputFormat =
        ../Internal/OutputFormat/Type sha256:90a967648257956b041755ef770b4d5a1adce53345a9443d872080c2f511ca2c
      ? ../Internal/OutputFormat/Type

let TaggedValueFormat =
        ../Internal/OutputFormat/TaggedValueFormat sha256:695155008931bf451f550cd682d0057920d1498dceac6e133774c6b200c6ba40
      ? ../Internal/OutputFormat/TaggedValueFormat

in    { tagOrder = Order.NameTagsExtension
      , openingSeparator = "["
      , leadingTagSeparator = False
      , tagSeparator = " "
      , taggedValueFormat = None TaggedValueFormat
      , trailingTagSeparator = False
      , closingSeparator = "]"
      }
    : OutputFormat
