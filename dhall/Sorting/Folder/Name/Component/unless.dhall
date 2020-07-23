{-
# Sorting.Folder.Name.Component.unless

## Component

Given a predicate and a list of components, evaluates to the list of components
if the predicate evaluates to false; otherwise evaluates to the empty string.

*Note:*  When the predicate is true, `unless` does not result in excessive
separators being put in the text.  For example, if the name template
is `[ text "A", unless always [ text "B", text "C" ], text "D" ]`, with
the separator `,`, the output will be `"A,D"`, not `"A,,,D"`.

## Examples

*
  File Tags: `[ "tag1", "tag2" ]`

  Component: `unless (hasTag "tag1") [ text "no tag 1" ]`

  Output:  `""`

*
  File Tags: `[ "tag1", "tag2" ]`

  Component: `unless (hasTag "tag1") [ text "no tag 3" ]`

  Output:  `"no tag 3"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.unless
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
      [ Autotagical.Sorting.Folder.Name.Component.text
          "Does not have the tag tag1"
      ]
```

-}

let List/map =
        https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? https://prelude.dhall-lang.org/List/map

let Case =
        ../../../../Internal/NameTemplate/Case sha256:9e2d5a3c338b63a4e067de578f9e65952648ed6ffcf4ee0caaa211cdb2b3a3c5
      ? ../../../../Internal/NameTemplate/Case

let Component =
        ../../../../Internal/NameTemplate/Folder/Component sha256:ed3197d74431e1d211f013f10b0cfcabf17c13084847cfdd7fc519cc551fa2bb
      ? ../../../../Internal/NameTemplate/Folder/Component

let Interpret =
        ../../../../Internal/NameTemplate/Interpret sha256:da352ee981f47618957222659071fb284597087a51e9426729b3fde383c77a91
      ? ../../../../Internal/NameTemplate/Interpret

let Predicate =
        ../../../../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../../../../Internal/Predicate/Type

let Tag =
        ../../../../Internal/Tag/Type sha256:3791aa7a52b92db3877472c57cec53ca856858bbad81156ef1bf29e9aeb2baee
      ? ../../../../Internal/Tag/Type

let unless
    : Predicate → List Component → Component
    = λ(predicate : Predicate) →
      λ(components : List Component) →
      λ(Component : Type) →
        let ComponentF =
              < FolderFormatAsF :
                  { componentsF : List Component, formatCaseF : Case }
              | FolderIfThenElseF :
                  { predicateF : Predicate
                  , trueComponentsF : List Component
                  , falseComponentsF : List Component
                  }
              | FolderInterpretF :
                  { componentF : Component, interpretAsF : Interpret }
              | FolderOriginalNameF
              | FolderTagValueF : Tag
              | FolderTextLiteralF : Text
              >

        in  λ(Fix : ComponentF → Component) →
              Fix
                ( ComponentF.FolderIfThenElseF
                    { predicateF = predicate
                    , trueComponentsF = [] : List Component
                    , falseComponentsF =
                        List/map
                          Component@1
                          Component
                          (λ(c : Component@1) → c Component Fix)
                          components
                    }
                )

in  unless
