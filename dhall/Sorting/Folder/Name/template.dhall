{-
# Sorting.Folder.Name.template

## Function

Given a list of name components, evaluate them to text based on a file's
information.

## Default Behavior

Components are concatenated directly without any spaces or other separators
between them.  To override this, use `Sorting.Name.With`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.template
      [ Autotagical.Sorting.Folder.Name.Component.originalName
      , Autotagical.Sorting.Folder.Name.Component.text "Has Tag 1"
      ]
```

-}

let Component =
        ../../../Internal/NameTemplate/Folder/Component sha256:ed3197d74431e1d211f013f10b0cfcabf17c13084847cfdd7fc519cc551fa2bb
      ? ../../../Internal/NameTemplate/Folder/Component

let NameTemplate =
        ../../../Internal/NameTemplate/Folder/Type sha256:15bf2c05808377075efee4ca436b514cfca6ee0d0bac8f1ff065dd3d4813d4f7
      ? ../../../Internal/NameTemplate/Folder/Type

let template
    : List Component → NameTemplate
    = λ(components : List Component) → { components, separator = None Text }

in  template
