{-|
# Predicate.hasGroup

## Predicate

Given a tag group, evaluates to true if one or more of the tags on the file is
in the group; otherwise, evaluates to false.

## Examples

Tags: `tag1`, `tag2`, `tag3`

Group1: `tag2`, `tag4`

Group2: `tag5`, `tag6`

* `hasGroup Group1: True`

* `hasGroup Group2: False`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Predicate.hasGroup
      ( Autotagical.Tags.group
          [ Autotagical.Tags.tag "tag1", Autotagical.Tags.tag "tag2" ]
      )
```

-}

let Bound =
        ../Internal/Predicate/Bound sha256:5df84f4de5c95a2e6e0cadea46dd4fcf4a0345e2190fe034f064351ede3ec30d
      ? ../Internal/Predicate/Bound

let Predicate =
        ../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../Internal/Predicate/Type

let Tag =
        ../Internal/Tag/Type sha256:3791aa7a52b92db3877472c57cec53ca856858bbad81156ef1bf29e9aeb2baee
      ? ../Internal/Tag/Type

let TagGroup =
        ../Internal/TagGroup/Type sha256:53d772dbc0ab2e509aa6425d923bc93e4fd75854cfdeb9245c37017f6c08bf29
      ? ../Internal/TagGroup/Type

let hasGroup
    : TagGroup → Predicate
    = λ(group : TagGroup) →
      λ(Predicate : Type) →
        let PredicateF =
              < AlwaysF
              | BetweenF :
                  { lowerBoundF : Bound
                  , upperBoundF : Bound
                  , predicatesF : List Predicate
                  }
              | HasGroupF : TagGroup
              | HasTagF : { tagF : Tag, withValueF : Optional (List Text) }
              | NotF : Predicate
              >

        in  λ(Fix : PredicateF → Predicate) → Fix (PredicateF.HasGroupF group)

in  hasGroup
