{-|
# Renaming.Name.template

## Function

Given a list of name components, evaluate them to text based on a file's
information.

## Default Behavior

Components are concatenated directly without any spaces or other separators
between them.  To override this, use `Renaming.Name.With`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.template
      [ Autotagical.Renaming.Component.originalName
      , Autotagical.Renaming.Component.text "Has Tag 1"
      ]
```

-}

let Component =
        ../../Internal/NameTemplate/File/Component sha256:b637988aae2ec7c1ee2b6fc7059008b03da8b57b1901dda79951c2045662bdfd
      ? ../../Internal/NameTemplate/File/Component

let NameTemplate =
        ../../Internal/NameTemplate/File/Type sha256:074ca643f1b18327ca8f4190432772b7460d0df888d3ee05ecaa7091f7c055da
      ? ../../Internal/NameTemplate/File/Type

let template
    : List Component → NameTemplate
    = λ(components : List Component) → { components, separator = None Text }

in  template
