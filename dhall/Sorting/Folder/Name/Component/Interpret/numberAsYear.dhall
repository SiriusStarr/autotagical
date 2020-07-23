{-
# Sorting.Folder.Name.Component.Interpret.numberAsYear

## Component

Given a year format, a natural number representing the most "modern" year to be
considered (relevant for 1- or 2-digit numbers), and a component, attempt to
interpret the component as a number and then output it as a year in the
requested format.

## Examples

*
  Format:  `1999`

  Latest Year: 2020

  Component: `"23"`

  Output: `"1923"`

*
  Format:  `1999`

  Latest Year: 2020

  Component: `"12"`

  Output: `"2012"`

*
  Format:  `99`

  Latest Year: 2020

  Component: `"23"`

  Output: `"23"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.Interpret.numberAsYear
      Autotagical.Sorting.Folder.Name.Component.Interpret.YearFormat.`99`
      2020
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

let YearFormat =
        ../../../../../Internal/NameTemplate/YearFormat sha256:b6943c82a96aebfff81eae71343d8727d5764c589668b48edf9e1c4cccf2d9ab
      ? ../../../../../Internal/NameTemplate/YearFormat

let numberAsYear
    : YearFormat → Natural → Component → Component
    = λ(yearFormat : YearFormat) →
      λ(latestYear : Natural) →
        interpret (Interpret.NumberAsYear { latestYear, yearFormat })

in  numberAsYear
