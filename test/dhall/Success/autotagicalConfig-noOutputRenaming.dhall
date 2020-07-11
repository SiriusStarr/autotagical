let A = ../../../dhall/package.dhall

in    A::{
      , inputFolders = [ "input" ]
      , inputFormat = A.InputFormat.tagSpacesFormat
      , inputPatterns =
          A.GlobPatterns.globPatterns [ "*.jpg", "*/*.png", "literal" ]
      , outputFolders = [ "output" ]
      , sortingSchema =
          A.Sorting.schema
            [ A.Sorting.Folder.folder
                "test1"
                A.Predicate.always
                [ A.Sorting.Folder.leaf
                    "test2"
                    (A.Predicate.not A.Predicate.always)
                ]
            ]
      }
    ⫽ A.Config.With.keepInputCopy
    ⫽ A.Config.With.dryRun
