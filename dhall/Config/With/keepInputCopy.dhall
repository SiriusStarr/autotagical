{-
# Config.With.keepInputCopy

## Default Behavior

By default, `autotagical` will **move** files from input folders, not copy them.

## Option Behavior

This option sets `autotagical` to **copy** files out of the input folders,
leaving the original files intact, rather than moving them.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.keepInputCopy
```

-}

{ keepCopyInInputFolder = True } : { keepCopyInInputFolder : Bool }
