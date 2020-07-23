let S = ../../../dhall/Sorting/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  S.schema
      [ S.Folder.leafFromText P.always "test1"
      , S.Folder.fromText
          P.always
          "test2"
          [ S.Folder.leafFromText P.always "child" ]
      ]
