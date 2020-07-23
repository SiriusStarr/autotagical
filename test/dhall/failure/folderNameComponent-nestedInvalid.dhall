let Component = ../../../dhall/Internal/NameTemplate/Folder/Component

let C = ../../../dhall/Sorting/Folder/Name/Component/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  C.Format.`As Title Case`
      [ C.ifThenElse P.always ([] : List Component) ([] : List Component) ]
