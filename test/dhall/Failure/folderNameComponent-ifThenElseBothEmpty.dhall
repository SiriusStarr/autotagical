let Component = ../../../dhall/Internal/NameTemplate/Folder/Component

let ifThenElse = ../../../dhall/SortingSchema/Folder/Name/Component/ifThenElse

let P = ../../../dhall/Predicate/package.dhall

in  ifThenElse P.always ([] : List Component) ([] : List Component)
