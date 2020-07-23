{-
# Sorting.Folder.Name.Component.Interpret.numberAsMonth

## Component

Given a month format and a component, attempt to interpret the component as a
number and then output it as a month in the requested format.

## Example

Format:  `Jan`

Component: `"9"`

Output: `"Sep"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.Interpret.numberAsMonth
      Autotagical.Sorting.Folder.Name.Component.Interpret.MonthFormat.January
      (Autotagical.Sorting.Folder.Name.Component.text "03")
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

let MonthFormat =
        ../../../../../Internal/NameTemplate/MonthFormat sha256:68591e45385f2983783395728381d90d988fae4c5194a187355131f84b1e1a0a
      ? ../../../../../Internal/NameTemplate/MonthFormat

let numberAsMonth
    : MonthFormat → Component → Component
    = λ(format : MonthFormat) → interpret (Interpret.NumberAsMonth format)

in  numberAsMonth
