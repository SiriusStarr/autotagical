{-|
# Renaming.Name.Component.duplicateNumber

## Component

If a file is not being deduplicated (when two files are placed in the same
folder with the same name and extension), evaluates to an empty string.  If the
file is being deduplicated, evaluates to the number of duplicate that the file
is (starting at 1).

*Note:*  There is no need to nest `duplicateNumber` inside of `ifDuplicate`,
though it is not problematic to do so.

*Note:*  When files are not duplicates, `duplicateNumber` does not result in
excessive separators being put in the text.  For example, if the name template
is `[ text "A", duplicateNumber, text "B" ]` with the separator `,`, and the
file is not a duplicate, the output will be `"A,B"`, not `"A,,B"`.

**Important:**  Duplicate numbering is based on the order of the files input
and as such **is** deterministic (i.e. if `autotagical` is run on the same input
multiple times, the same files will be assigned to the same duplicate numbers).
It should **not** be relied on to be idempotent (i.e. if rerun on the
now-renamed files, the duplicate numbers would be the same), since this depends
considerably on the name templates being used, etc.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.Name.Component.duplicateNumber
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

let duplicateNumber
    : Component
    = λ(Component : Type) →
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
              Fix ComponentF.FileDuplicateNumberF

in  duplicateNumber
