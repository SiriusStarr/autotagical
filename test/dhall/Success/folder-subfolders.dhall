let F = ../../../dhall/Sorting/Folder/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  F.folder
      "parent"
      P.always
      [ F.leaf "child1" (P.not P.always)
      , F.fromTemplate
          (F.Name.template [ F.Name.Component.text "child2" ])
          (P.not P.always)
          [ F.leaf "grandchild" P.always ]
      ]
