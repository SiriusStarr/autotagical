{-
# Config.With.newOutputFormat

## Default Behavior

By default, `autotagical` will not modify the tagging format of files in any
way.

## Option Behavior

This option instructs `autotagical` to rewrite tags from the input format into a
specified output format.  This is useful if you have, for example, files that
were tagged in one format but you want to re-tag them in a new format.

It will also alphabetize tag order and deduplicate tags if called with the same
output format as input format.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical::{...}
    ⫽ Autotagical.Config.With.newOutputFormat
        (Autotagical.Output.format "{" "," "}")
```

-}

let OutputFormat =
        ../../Internal/OutputFormat/Type sha256:90a967648257956b041755ef770b4d5a1adce53345a9443d872080c2f511ca2c
      ? ../../Internal/OutputFormat/Type

let newOutputFormat
    : OutputFormat → { outputFormat : Optional OutputFormat }
    = λ(format : OutputFormat) → { outputFormat = Some format }

in  newOutputFormat
