let P = ../../../dhall/Predicate/package.dhall

let R = ../../../dhall/RenamingSchema/package.dhall

in  R.rule
      P.always
      ( R.Name.template
          [ R.Name.Component.text "test"
          , R.Name.Component.originalName
          , R.Name.Component.duplicateNumber
          ]
      )
