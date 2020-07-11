let P = ../../../dhall/Predicate/package.dhall

let R = ../../../dhall/Renaming/package.dhall

in  R.schema
      [ R.rule
          P.always
          ( R.Name.template
              [ R.Name.Component.text "test1"
              , R.Name.Component.originalName
              , R.Name.Component.duplicateNumber
              ]
          )
      , R.rule
          (P.not P.always)
          ( R.Name.template
              [ R.Name.Component.text "test2", R.Name.Component.originalName ]
          )
      ]
