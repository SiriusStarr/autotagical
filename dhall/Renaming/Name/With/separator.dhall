{-|
# Renaming.Name.With.separator

## Default Behavior

Components are concatenated directly without any spaces or other separators
between them.  To override this, use `Renaming.Name.With`.

## Option Behavior

Given a supplied text separator, intercalate it between components, e.g. if the
separator is a space, a space will be placed between each component (but not at
either end).

Components: `[ "component1", "component2", "component3" ]`

Separator: `","`

Result: `"component1,component2,component3"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in    Autotagical.Renaming.template
        [ Autotagical.Renaming.Component.originalName
        , Autotagical.Renaming.Component.text "Has Tag 1"
        ]
    ⫽ Autotagical.Renaming.With.separator ","
```

-}

let separator
    : Text → { separator : Optional Text }
    = λ(separator : Text) → { separator = Some separator }

in  separator
