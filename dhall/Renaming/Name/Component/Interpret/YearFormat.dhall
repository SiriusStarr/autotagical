{-
# Renaming.Name.Component.Interpret.YearFormat

## Formats

Specifies the format to write a year as for
`Renaming.Name.Component.Interpret.numberAsYear`.

* `99`:

  `4 -> 04`

  `1843 -> 43`

* `1999`:

  `4 -> 2004` (Depending on latest year passed to `numberAsYear`)

  `1754 -> 1754`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.Interpret.YearFormat.January
```

-}

let YearFormat =
        ../../../../Internal/NameTemplate/YearFormat sha256:b6943c82a96aebfff81eae71343d8727d5764c589668b48edf9e1c4cccf2d9ab
      ? ../../../../Internal/NameTemplate/YearFormat

in    { `99` = YearFormat.TwoDigitYear, `1999` = YearFormat.FourDigitYear }
    : { `99` : YearFormat, `1999` : YearFormat }
