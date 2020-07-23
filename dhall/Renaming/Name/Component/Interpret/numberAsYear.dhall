{-
# Renaming.Name.Component.Interpret.numberAsYear

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

in  Autotagical.Renaming.Name.Component.Interpret.numberAsYear
      Autotagical.Renaming.Name.Component.Interpret.YearFormat.`99`
      2020
      (Autotagical.Renaming.Name.Component.text "03")
```

-}

let Component =
        ../../../../Internal/NameTemplate/File/Component sha256:b637988aae2ec7c1ee2b6fc7059008b03da8b57b1901dda79951c2045662bdfd
      ? ../../../../Internal/NameTemplate/File/Component

let interpret =
        ../../../../Internal/NameTemplate/File/interpret sha256:a85ca124c676f97c1ae527e80e00dba5767ffe772143ab5672898f1cba830b31
      ? ../../../../Internal/NameTemplate/File/interpret

let Interpret =
        ../../../../Internal/NameTemplate/Interpret sha256:da352ee981f47618957222659071fb284597087a51e9426729b3fde383c77a91
      ? ../../../../Internal/NameTemplate/Interpret

let YearFormat =
        ../../../../Internal/NameTemplate/YearFormat sha256:b6943c82a96aebfff81eae71343d8727d5764c589668b48edf9e1c4cccf2d9ab
      ? ../../../../Internal/NameTemplate/YearFormat

let numberAsYear
    : YearFormat → Natural → Component → Component
    = λ(yearFormat : YearFormat) →
      λ(latestYear : Natural) →
        interpret (Interpret.NumberAsYear { latestYear, yearFormat })

in  numberAsYear
