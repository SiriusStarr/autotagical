{-
# Sorting.Folder.Name.Component.originalName

## Component

Evaluates to the complete original file name (but not extension or tags) of an
input file.  Useful if you want to preserve original file names but also add to
them in some fashion.

## Example

File Name: `"document [tag1 tag2].pdf"`

Name Template: `[ originalName, text "renamed" ]`

Output:  `"document renamed"`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.Name.Component.originalName
```

-}

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

let originalName
    : Component
    = λ(Component : Type) →
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

        in  λ(Fix : ComponentF → Component) → Fix ComponentF.FolderOriginalNameF

in  originalName
