{-|
# Renaming.Name.Component.when

## Component

Given a predicate and a list of components, evaluates to the list of components
if the predicate evaluates to true; otherwise evaluates to the empty string.

*Note:*  When the predicate is false, `when` does not result in excessive
separators being put in the text.  For example, if the name template
is `[ text "A", when (not always) [ text "B", text "C" ], text "D" ]`, with
the separator `,`, the output will be `"A,D"`, not `"A,,,D"`.

## Examples

*
  File Tags: `[ "tag1", "tag2" ]`

  Component: `when (hasTag "tag1") [ text "has tag 1" ]`

  Output:  `"has tag 1"`

*
  File Tags: `[ "tag1", "tag2" ]`

  Component: `unless (hasTag "tag1") [ text "has tag 3" ]`

  Output:  `""`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.when
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
      [ Autotagical.Renaming.Name.Component.text "Does have the tag tag1" ]
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

let when
    : Predicate → List Component → Component
    = λ(predicate : Predicate) →
      λ(components : List Component) →
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
                ( ComponentF.FileIfThenElseF
                    { predicateF = predicate
                    , trueComponentsF =
                        List/map
                          Component@1
                          Component
                          (λ(c : Component@1) → c Component Fix)
                          components
                    , falseComponentsF = [] : List Component
                    }
                )

in  when
