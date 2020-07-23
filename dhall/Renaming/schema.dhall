{-
# Renaming.schema

## Function

Given a list of renaming rules, renames unnamed files based on them.  Files will
be renamed according to the **first** rule for with the predicate evaluates to
true (i.e. the order of the list of rules matters).

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Renaming.schema
      [ Autotagical.Renaming.rule
          (Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1"))
          ( Autotagical.Renaming.template
              [ Autotagical.Renaming.Component.text "Has Tag 1" ]
          )
      , Autotagical.Renaming.rule
          Autotagical.Predicate.always
          ( Autotagical.Renaming.template
              [ Autotagical.Renaming.Component.text "Does Not Have Tag 1" ]
          )
      ]
```

-}

let RenamingRule =
        ../Internal/RenamingSchema/RenamingRule sha256:c695182d59a0befe6ed295955ef4aad0aab4eb8485ae81151e85971f9a44b28e
      ? ../Internal/RenamingSchema/RenamingRule

let RenamingSchema =
        ../Internal/RenamingSchema/Type sha256:10694e04e153916ee5907f3aef8882da66dbff54996ca2b54b97ae27e38fdefc
      ? ../Internal/RenamingSchema/Type

let schema
    : List RenamingRule → RenamingSchema
    = λ(rules : List RenamingRule) → rules

in  schema
