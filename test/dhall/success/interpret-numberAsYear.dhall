let Interpret = ../../../dhall/Internal/NameTemplate/Interpret

let YearFormat =
      ../../../dhall/Renaming/Name/Component/Interpret/YearFormat.dhall

in  Interpret.NumberAsYear { latestYear = 2021, yearFormat = YearFormat.`1999` }
