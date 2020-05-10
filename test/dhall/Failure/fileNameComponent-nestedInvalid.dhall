let Component = ../../../dhall/Internal/NameTemplate/File/Component

let C = ../../../dhall/RenamingSchema/Name/Component/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  C.Format.`As Title Case`
      (C.ifThenElse P.always ([] : List Component) ([] : List Component))
