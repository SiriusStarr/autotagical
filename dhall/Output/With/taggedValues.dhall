{-|
# Output.With.taggedValues

## Default Behavior

By default, output formats forbid tagged values.

## Option Behavior

This option sets the output format to write tagged values when encountered
(possibly as well as pure tags).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Output.tagSpacesFormat
    ⫽ Autotagical.Output.With.taggedValues
        (Autotagical.Output.TaggedValueFormat.`tag=value` False)
```

-}

let TaggedValueFormat =
        ../../Internal/OutputFormat/TaggedValueFormat sha256:695155008931bf451f550cd682d0057920d1498dceac6e133774c6b200c6ba40
      ? ../../Internal/OutputFormat/TaggedValueFormat

let taggedValues
    : TaggedValueFormat → { taggedValueFormat : Optional TaggedValueFormat }
    = λ(format : TaggedValueFormat) → { taggedValueFormat = Some format }

in  taggedValues
