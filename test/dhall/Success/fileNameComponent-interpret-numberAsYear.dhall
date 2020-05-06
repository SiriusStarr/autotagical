let F = ../../../dhall/RenamingSchema/Name/Component/package.dhall

in  F.Interpret.numberAsYear (F.text "01") F.Interpret.YearFormat.`1999` 99
