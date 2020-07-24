{-|
# Sorting.Folder.Name.Component.Interpret.numberAsOrdinal

## Component

Given a component, attempt to interpret the component as a number and then
output it as an ordinal (e.g. "2nd").

## Examples

* `1 -> 1st`

* `13 -> 13th`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.Interpret.numberAsOrdinal
      (Autotagical.Sorting.Folder.Name.Component.text "3")
```

-}

let Component =
        ../../../../../Internal/NameTemplate/Folder/Component sha256:ed3197d74431e1d211f013f10b0cfcabf17c13084847cfdd7fc519cc551fa2bb
      ? ../../../../../Internal/NameTemplate/Folder/Component

let interpret =
        ../../../../../Internal/NameTemplate/Folder/interpret sha256:e676cb92241e73a01fe5868814865d2a080c10b607d82b32c9c607dfe5994aa1
      ? ../../../../../Internal/NameTemplate/Folder/interpret

let Interpret =
        ../../../../../Internal/NameTemplate/Interpret sha256:da352ee981f47618957222659071fb284597087a51e9426729b3fde383c77a91
      ? ../../../../../Internal/NameTemplate/Interpret

in  interpret Interpret.NumberAsOrdinal : Component → Component
