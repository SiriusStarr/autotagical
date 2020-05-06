let S = ../../../dhall/SortingSchema/package.dhall

let P = ../../../dhall/Predicate/package.dhall

in  S.sortingSchema
      [ S.Folder.leaf "test1" P.always
      , S.Folder.folder "test2" P.always [ S.Folder.leaf "child" P.always ]
      ]
