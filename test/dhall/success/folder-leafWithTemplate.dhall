let F = ../../../dhall/Sorting/Folder/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  F.leafFromTemplate
      P.always
      (   F.Name.template [ F.Name.Component.text "test" ]
        ⫽ F.Name.With.separator " "
      )
