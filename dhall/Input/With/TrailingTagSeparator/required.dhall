{-|
# Input.With.TrailingTagSeparator.required

## Default Behavior

By default, trailing tag separators are optional.  For example, `[tag1,tag2,]`
and `[tag1,tag2]` are both valid, if the tag separator is `,`

## Option Behavior

This option sets the input format to be more strict and instead specifically
require trailing tag separators.  For example, `[tag1,tag2]` would fail to parse
in the above example.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Input.tagSpacesFormat
    ⫽ Autotagical.Input.With.TrailingTagSeparator.required
```

-}

let SeparatorRequirement =
        ../../../Internal/InputFormat/SeparatorRequirement sha256:047b627a9979b467750a376b4f61681cd1071d132c71007fcd2f7a00df993a17
      ? ../../../Internal/InputFormat/SeparatorRequirement

in    { trailingTagSeparator = SeparatorRequirement.Required }
    : { trailingTagSeparator : SeparatorRequirement }
