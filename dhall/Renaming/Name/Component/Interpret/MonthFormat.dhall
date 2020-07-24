{-|
# Renaming.Name.Component.Interpret.MonthFormat

## Formats

Specifies the format to write a month as for
`Renaming.Name.Component.Interpret.numberAsMonth`.

* `1`:

  `4 -> 4`

  `10 -> 10`

* `01`:

  `4 -> 04`

  `10 -> 10`

* `Jan`:

  `4 -> Apr`

  `10 -> Oct`

* `January`:

  `4 -> April`

  `10 -> October`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.Interpret.MonthFormat.January
```

-}

let MonthFormat =
        ../../../../Internal/NameTemplate/MonthFormat sha256:68591e45385f2983783395728381d90d988fae4c5194a187355131f84b1e1a0a
      ? ../../../../Internal/NameTemplate/MonthFormat

in    { `1` = MonthFormat.OneDigitMonth
      , `01` = MonthFormat.TwoDigitMonth
      , Jan = MonthFormat.ThreeLetterMonth
      , January = MonthFormat.MonthName
      }
    : { `1` : MonthFormat
      , `01` : MonthFormat
      , Jan : MonthFormat
      , January : MonthFormat
      }
