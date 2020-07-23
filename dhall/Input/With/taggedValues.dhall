{-
# Input.With.taggedValues

## Default Behavior

By default, input formats only recognize pure tags.

## Option Behavior

This option sets the input format to expect tagged values (either instead of or
in addition to pure tags).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Input.tagSpacesFormat
    ⫽ Autotagical.Input.With.taggedValues
        (Autotagical.Input.TaggedValueFormat.`tag=value` False)
```

-}

let TaggedValueFormat =
        ../../Internal/InputFormat/TaggedValueFormat sha256:62b80103bf88b5ca5f35005b578267a5679fdbdcfb0b77229c3651b882961551
      ? ../../Internal/InputFormat/TaggedValueFormat

let taggedValues
    : TaggedValueFormat → { taggedValueFormat : Optional TaggedValueFormat }
    = λ(format : TaggedValueFormat) → { taggedValueFormat = Some format }

in  taggedValues
