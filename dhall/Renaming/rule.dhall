{-
# Renaming.rule

## Function

Given a predicate and a name template, renames files for which the predicate
evaluates to true using the name template.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.rule
      (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
      ( Autotagical.Renaming.template
          [ Autotagical.Renaming.Component.text "Has Tag 1" ]
      )
```

-}

let NameTemplate =
        ../Internal/NameTemplate/File/Type sha256:074ca643f1b18327ca8f4190432772b7460d0df888d3ee05ecaa7091f7c055da
      ? ../Internal/NameTemplate/File/Type

let Predicate =
        ../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../Internal/Predicate/Type

let RenamingRule =
        ../Internal/RenamingSchema/RenamingRule sha256:c695182d59a0befe6ed295955ef4aad0aab4eb8485ae81151e85971f9a44b28e
      ? ../Internal/RenamingSchema/RenamingRule

let rule
    : Predicate → NameTemplate → RenamingRule
    = λ(predicate : Predicate) → λ(name : NameTemplate) → { name, predicate }

in  rule
