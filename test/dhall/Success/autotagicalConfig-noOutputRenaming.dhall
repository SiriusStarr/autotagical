let A = ../../../dhall/package.dhall

in    A::{
      , inputFolders = [ "input" ]
      , inputFormat = A.InputFormat.tagSpacesFormat
      , inputPatterns =
          A.GlobPatterns.globPatterns [ "*.jpg", "*/*.png", "literal" ]
      , outputFolders = [ "output" ]
      , sortingSchema =
          A.SortingSchema.sortingSchema
            [ A.SortingSchema.Folder.folder
                "test1"
                A.Predicate.always
                [ A.SortingSchema.Folder.leaf
                    "test2"
                    (A.Predicate.not A.Predicate.always)
                ]
            ]
      }
    ⫽ A.Config.With.keepInputCopy
