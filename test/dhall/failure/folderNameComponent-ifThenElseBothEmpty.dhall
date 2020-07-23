let Component = ../../../dhall/Internal/NameTemplate/Folder/Component

let ifThenElse = ../../../dhall/Sorting/Folder/Name/Component/ifThenElse.dhall

let P = ../../../dhall/Predicate/package.dhall

in  ifThenElse P.always ([] : List Component) ([] : List Component)
