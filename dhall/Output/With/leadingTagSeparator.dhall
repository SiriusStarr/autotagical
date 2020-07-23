{-
# Output.With.leadingTagSeparator

## Default Behavior

By default, leading tag separators are not written, e.g. `[tag1,tag2]` rather
than `[,tag1,tag2]`, if the tag separator is `,`

## Option Behavior

This option sets the output format to write leading tag separators, e.g. the
above would be written as `[,tag1,tag2]`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Output.tagSpacesFormat
    ⫽ Autotagical.Output.With.leadingTagSeparator
```

-}

{ leadingTagSeparator = True } : { leadingTagSeparator : Bool }
