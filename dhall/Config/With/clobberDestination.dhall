{-|
# Config.With.clobberDestination

## Default Behavior

By default, `autotagical` will not overwrite files already in the destination
folder and will throw a warning (and not move the file) instead.

## Option Behavior

This option sets `autotagical` to clobber (overwrite) destination files in
output folders.  To set this, you must call it with a specific text value.

This is to force acknowledgement that **data loss may occur**.  Be very careful
that you truly don't care about any data in your output folders before setting
this option.

## Usage

```
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical::{...}
    ⫽ Autotagical.Config.With.clobberDestination
        "I understand that data loss may occur; I want to overwrite destination files."
```

-}

let clobberDestination
    : Text → { clobberDestination : Optional Text }
    = λ(acknowledgment : Text) → { clobberDestination = Some acknowledgment }

in  clobberDestination
