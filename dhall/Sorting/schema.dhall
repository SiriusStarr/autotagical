{-|
# Sorting.schema

## Function

Given a list of folders, sorts files based on them.  Files will be sorted into
the first folder (at each level) for which the predicate evaluates to true (i.e.
the orders of the lists of folders matter).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Sorting.schema
      [ Autotagical.Sorting.Folder.leaf
          "test1"
          (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
      , Autotagical.Sorting.Folder.fromText
          "test2"
          Autotagical.Predicate.always
          [ Autotagical.Sorting.Folder.leaf "child" Autotagical.Predicate.always
          ]
      ]
```

-}

let Folder =
        ../Internal/SortingSchema/Folder sha256:9ac6f99457ac6d06ff98d253a5b849f623700d8bd30f6c67900650d4430f1f5c
      ? ../Internal/SortingSchema/Folder

let SortingSchema =
        ../Internal/SortingSchema/Type sha256:8803cab12cfc2a91454cca250e7a03f20c23ee400275512d0c924390bc9f0a73
      ? ../Internal/SortingSchema/Type

let schema
    : List Folder → SortingSchema
    = λ(fs : List Folder) → fs

in  schema
