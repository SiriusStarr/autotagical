let A = ../../../dhall/package.dhall

in    A::{
      , inputFolders = [ "input" ]
      , inputFormat = A.Input.tagSpacesFormat
      , inputPatterns = A.Glob.patterns [ "*.jpg", "*/*.png", "literal" ]
      , outputFolders = [ "output" ]
      , sortingSchema =
          A.Sorting.schema
            [ A.Sorting.Folder.fromText
                A.Predicate.always
                "test1"
                [ A.Sorting.Folder.leafFromText
                    (A.Predicate.not A.Predicate.always)
                    "test2"
                ]
            ]
      }
    ⫽ A.Config.With.ignorePatterns (A.Glob.patterns [ ".*" ])
    ⫽ A.Config.With.newOutputFormat (A.Output.format "{" "," "}")
    ⫽ A.Config.With.renaming
        (A.Glob.patterns [ "unnamed*" ] ⫽ A.Glob.With.errorRecovery)
        ( A.Renaming.schema
            [ A.Renaming.rule
                A.Predicate.always
                ( A.Renaming.Name.template
                    [ A.Renaming.Name.Component.text "rename" ]
                )
            ]
        )
