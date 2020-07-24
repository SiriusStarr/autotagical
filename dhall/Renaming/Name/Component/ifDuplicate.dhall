{-|
# Renaming.Name.Component.ifDuplicate

## Component

Given a list of components, if a file is not being deduplicated (when two files
are placed in the same folder with the same name and extension), evaluates to
the empty string.  If the file is being deduplicated, evaluates to that list of
components.

*Note:*  There is no need to nest `duplicateNumber` inside of `ifDuplicate`,
though it is not problematic to do so.

*Note:*  When files are not duplicates, `ifDuplicate` does not result in
excessive separators being put in the text.  For example, if the name template
is `[ text "A", ifDuplicate [ text "B", text "C" ], text "D" ]` with the
separator `,`, and the file is not a duplicate, the output will be `"A,D"`, not
`"A,,,D"`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.ifDuplicate
      [ Autotagical.Renaming.Name.Component.text "Duplicate number: "
      , Autotagical.Renaming.Name.Component.duplicateNumber
      ]
```

-}

let List/map =
        https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? https://prelude.dhall-lang.org/List/map

let Case =
        ../../../Internal/NameTemplate/Case sha256:9e2d5a3c338b63a4e067de578f9e65952648ed6ffcf4ee0caaa211cdb2b3a3c5
      ? ../../../Internal/NameTemplate/Case

let Component =
        ../../../Internal/NameTemplate/File/Component sha256:b637988aae2ec7c1ee2b6fc7059008b03da8b57b1901dda79951c2045662bdfd
      ? ../../../Internal/NameTemplate/File/Component

let Interpret =
        ../../../Internal/NameTemplate/Interpret sha256:da352ee981f47618957222659071fb284597087a51e9426729b3fde383c77a91
      ? ../../../Internal/NameTemplate/Interpret

let Predicate =
        ../../../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../../../Internal/Predicate/Type

let Tag =
        ../../../Internal/Tag/Type sha256:3791aa7a52b92db3877472c57cec53ca856858bbad81156ef1bf29e9aeb2baee
      ? ../../../Internal/Tag/Type

let ifDuplicate
    : List Component → Component
    = λ(components : List Component) →
      λ(Component : Type) →
        let ComponentF =
              < FileDuplicateNumberF
              | FileFormatAsF :
                  { componentsF : List Component, formatCaseF : Case }
              | FileIfDuplicateF : List Component
              | FileIfThenElseF :
                  { predicateF : Predicate
                  , trueComponentsF : List Component
                  , falseComponentsF : List Component
                  }
              | FileInterpretF :
                  { componentF : Component, interpretAsF : Interpret }
              | FileOriginalNameF
              | FileTagValueF : Tag
              | FileTextLiteralF : Text
              >

        in  λ(Fix : ComponentF → Component) →
              Fix
                ( ComponentF.FileIfDuplicateF
                    ( List/map
                        Component@1
                        Component
                        (λ(c : Component@1) → c Component Fix)
                        components
                    )
                )

in  ifDuplicate
