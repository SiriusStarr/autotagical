let F = ../../../dhall/Sorting/Folder/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  F.fromText
      P.always
      "parent"
      [ F.leafFromText (P.not P.always) "child1"
      , F.fromTemplate
          (P.not P.always)
          (F.Name.template [ F.Name.Component.text "child2" ])
          [ F.leafFromText P.always "grandchild" ]
      ]
