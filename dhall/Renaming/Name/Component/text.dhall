{-|
# Renaming.Name.Component.text

## Component

Given nonempty text, evaluates to that literal text.  This is the most
fundamental component in a name template.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.text "A literal text string."
```

-}

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

let text
    : Text → Component
    = λ(text : Text) →
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
              Fix (ComponentF.FileTextLiteralF text)

in  text
