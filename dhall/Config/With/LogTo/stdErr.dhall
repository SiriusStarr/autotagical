{-|
# Config.With.LogTo.stdErr

## Default Behavior

By default, `autotagical` writes logs to `stdout`.

## Option Behavior

This option writes log messages to `stderr`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in Autotagical::{...} ⫽ Autotagical.Config.With.LogTo.stdErr
```

-}

let LogDestination =
        ../../../Internal/Logging/LogDestination sha256:47393d4c5bdc46697670d9b6d4c73c4875c1df2f2cf2f1cbe68b2afe03c0816f
      ? ../../../Internal/Logging/LogDestination

in    { logDestination = LogDestination.StdErr }
    : { logDestination : LogDestination }
