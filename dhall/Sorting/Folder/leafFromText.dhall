{-
# Sorting.Folder.leafFromText

## Function

Given a predicate and a text name , create a folder with no subfolders with that
literal text name and predicate.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.leafFromText
      "Leaf folder name"
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
```

-}

let Folder =
        ../../Internal/SortingSchema/Folder sha256:9ac6f99457ac6d06ff98d253a5b849f623700d8bd30f6c67900650d4430f1f5c
      ? ../../Internal/SortingSchema/Folder

let template =
        ./Name/template.dhall sha256:878214add5443ce8ff53da331392d12ec96d0c6e3c584d127adf74d85562edef
      ? ./Name/template.dhall

let NameTemplate =
        ../../Internal/NameTemplate/Folder/Type sha256:15bf2c05808377075efee4ca436b514cfca6ee0d0bac8f1ff065dd3d4813d4f7
      ? ../../Internal/NameTemplate/Folder/Type

let Predicate =
        ../../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../../Internal/Predicate/Type

let text =
        ./Name/Component/text.dhall sha256:8476317c271490fb0f5f27395ef853087277f0ae1da3788952f6a67c3d99f682
      ? ./Name/Component/text.dhall

let leafFromText
    : Predicate → Text → Folder
    = λ(predicate : Predicate) →
      λ(name : Text) →
      λ(Folder : Type) →
      λ ( Fix
        : { nameF : NameTemplate
          , predicateF : Predicate
          , subfoldersF : List Folder
          } →
            Folder
        ) →
        Fix
          { nameF = template [ text name ]
          , predicateF = predicate
          , subfoldersF = [] : List Folder
          }

in  leafFromText
