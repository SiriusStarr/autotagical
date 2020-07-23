{-
# Renaming.Name.Component.Interpret.numberAsMonth

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

in  Autotagical.Renaming.Name.Component.Interpret.numberAsMonth
      Autotagical.Renaming.Name.Component.Interpret.MonthFormat.January
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

let MonthFormat =
        ../../../../Internal/NameTemplate/MonthFormat sha256:68591e45385f2983783395728381d90d988fae4c5194a187355131f84b1e1a0a
      ? ../../../../Internal/NameTemplate/MonthFormat

let numberAsMonth
    : MonthFormat → Component → Component
    = λ(format : MonthFormat) → interpret (Interpret.NumberAsMonth format)

in  numberAsMonth
