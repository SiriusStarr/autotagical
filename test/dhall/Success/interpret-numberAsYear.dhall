let Interpret = ../../../dhall/Internal/NameTemplate/Interpret

let YearFormat =
      ../../../dhall/RenamingSchema/Name/Component/Interpret/YearFormat

in  Interpret.NumberAsYear { yearFormat = YearFormat.`1999`, centurySplit = 22 }
