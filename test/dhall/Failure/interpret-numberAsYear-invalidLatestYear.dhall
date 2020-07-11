let Interpret = ../../../dhall/Internal/NameTemplate/Interpret

let YearFormat = ../../../dhall/Renaming/Name/Component/Interpret/YearFormat

in  Interpret.NumberAsYear { latestYear = 0, yearFormat = YearFormat.`1999` }
