let Component = ../../../dhall/Internal/NameTemplate/File/Component

let ifThenElse = ../../../dhall/Renaming/Name/Component/ifThenElse

let P = ../../../dhall/Predicate/package.dhall

in  ifThenElse P.always ([] : List Component) ([] : List Component)
