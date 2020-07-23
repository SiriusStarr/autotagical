let Interpret = ../../../dhall/Internal/NameTemplate/Interpret

let MonthFormat =
      ../../../dhall/Renaming/Name/Component/Interpret/MonthFormat.dhall

in  Interpret.NumberAsMonth MonthFormat.January
