{-
# Config.With.dryRun

## Default Behavior

By default, `autotagical` will move files, since that is its purpose.

## Option Behavior

This option sets `autotagical` to perform all steps except actually manipulating
the filesystem in any way.  This is useful with `Config.With.LogLevel.info` to
see the outcome of your settings without actually moving anything.  Note that
this will not necessarily detect *every* problem, as it won't detect permissions
issues, for example.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.dryRun
```

-}

{ dryRun = True } : { dryRun : Bool }
