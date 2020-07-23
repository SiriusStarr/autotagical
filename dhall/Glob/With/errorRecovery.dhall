{-
# Glob.With.errorRecovery

## Default Behavior

By default, glob patterns will fail to compile if invalid syntax is encountered.
This will result in the Dhall configuration failing to load (so no risk of
your patterns accidentally doing the wrong thing).

## Option Behavior

This option turns on error recovery for patterns.  With error recovery, if any
part of a pattern is invalid, it is simply interpreted as literal characters.

For example, the pattern `[abc` fails to compile without error recovery, as it
contains an incomplete character range.  With error recovery, the pattern will
compile and will just match the literal sequence of characters `[abc`.

See [System.FilePath.Glob](http://hackage.haskell.org/package/Glob-0.10.0/docs/System-FilePath-Glob.html#t:CompOptions)
for more details.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Glob.patterns [ "[abc" ] ⫽ Autotagical.Glob.With.errorRecovery
```

-}

{ errorRecovery = True } : { errorRecovery : Bool }
