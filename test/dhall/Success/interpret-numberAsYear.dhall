let Interpret = ../../../dhall/Internal/NameTemplate/Interpret

let YearFormat =
      ../../../dhall/RenamingSchema/Name/Component/Interpret/YearFormat

in  Interpret.NumberAsYear { latestYear = 2021, yearFormat = YearFormat.`1999` }
