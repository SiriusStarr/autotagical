{-
# Sorting.Folder.leafFromTemplate

## Function

Given a predicate and a name template, create a folder with no subfolders with
that name template (which will be evaluated based on a file sorted to it) and
predicate.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.leafFromTemplate
      ( Autotagical.Sorting.Folder.Name.template
          [ Autotagical.Sorting.Folder.Name.Component.originalName
          , Autotagical.Sorting.Folder.Name.Component.text "Has Tag 1"
          ]
      )
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
```

-}

let Folder =
        ../../Internal/SortingSchema/Folder sha256:9ac6f99457ac6d06ff98d253a5b849f623700d8bd30f6c67900650d4430f1f5c
      ? ../../Internal/SortingSchema/Folder

let NameTemplate =
        ../../Internal/NameTemplate/Folder/Type sha256:15bf2c05808377075efee4ca436b514cfca6ee0d0bac8f1ff065dd3d4813d4f7
      ? ../../Internal/NameTemplate/Folder/Type

let Predicate =
        ../../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../../Internal/Predicate/Type

let leafFromTemplate
    : Predicate → NameTemplate → Folder
    = λ(predicate : Predicate) →
      λ(name : NameTemplate) →
      λ(Folder : Type) →
      λ ( Fix
        : { nameF : NameTemplate
          , predicateF : Predicate
          , subfoldersF : List Folder
          } →
            Folder
        ) →
        Fix
          { nameF = name
          , predicateF = predicate
          , subfoldersF = [] : List Folder
          }

in  leafFromTemplate
