{-
# Sorting.Folder.fromTemplate

## Function

Given a predicate, a name template, and a list of subfolders, create a folder
with the name template (which will be evaluated based on a file sorted to it),
predicate, and subfolders.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.Folder.fromTemplate
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
      ( Autotagical.Sorting.Folder.Name.template
          [ Autotagical.Sorting.Folder.Name.Component.originalName
          , Autotagical.Sorting.Folder.Name.Component.text "Has Tag 1"
          ]
      )
      [ Autotagical.Sorting.Folder.leaf "Child" Autotagical.Predicate.always ]
```

-}

let List/map =
        https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? https://prelude.dhall-lang.org/List/map

let Folder =
        ../../Internal/SortingSchema/Folder sha256:9ac6f99457ac6d06ff98d253a5b849f623700d8bd30f6c67900650d4430f1f5c
      ? ../../Internal/SortingSchema/Folder

let NameTemplate =
        ../../Internal/NameTemplate/Folder/Type sha256:15bf2c05808377075efee4ca436b514cfca6ee0d0bac8f1ff065dd3d4813d4f7
      ? ../../Internal/NameTemplate/Folder/Type

let Predicate =
        ../../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../../Internal/Predicate/Type

let fromTemplate
    : Predicate → NameTemplate → List Folder → Folder
    = λ(predicate : Predicate) →
      λ(name : NameTemplate) →
      λ(subfolders : List Folder) →
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
          , subfoldersF =
              List/map
                Folder@1
                Folder
                (λ(f : Folder@1) → f Folder Fix)
                subfolders
          }

in  fromTemplate
