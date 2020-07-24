{-|
# Output.With.trailingTagSeparator

## Default Behavior

By default, trailing tag separators are not written, e.g. `[tag1,tag2]` rather
than `[tag1,tag2,]`, if the tag separator is `,`

## Option Behavior

This option sets the output format to write trailing tag separators, e.g. the
above would be written as `[tag1,tag2,]`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Output.tagSpacesFormat
    ⫽ Autotagical.Output.With.trailingTagSeparator
```

-}

{ trailingTagSeparator = True } : { trailingTagSeparator : Bool }
