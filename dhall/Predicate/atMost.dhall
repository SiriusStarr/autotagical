{-|
# Predicate.atMost

## Predicate

Given a natural number `N` and nonempty list of predicates, evaluates to true if
`N` or fewer of the predicates evaluate to true; otherwise, evaluates to false.

## Examples

Tags: `tag1`, `tag2`, `tag3`

* `atMost 2 [hasTag "tag1", hasTag "tag2", hasTag "tag3", hasTag "tag4"]: False`

* `atMost 2 [hasTag "tag1", hasTag "tag2", hasTag "tag4"]: True`

* `atMost 2 [hasTag "tag1", hasTag "tag4"]: True`

* `atMost 2 [hasTag "tag4"]: True`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Predicate.atLeast
      2
      [ Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1")
      , Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag2")
      ]
```

-}

let between =
        ../Internal/Predicate/between sha256:d9e26bcb027406f03cb1b0cefc0faadbaea36ddf516d6f0ed6bad7476ae27129
      ? ../Internal/Predicate/between

let Bound =
        ../Internal/Predicate/Bound sha256:5df84f4de5c95a2e6e0cadea46dd4fcf4a0345e2190fe034f064351ede3ec30d
      ? ../Internal/Predicate/Bound

let Predicate =
        ../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../Internal/Predicate/Type

let atMost
    : Natural → List Predicate → Predicate
    = λ(upperBound : Natural) →
      λ(predicates : List Predicate) →
        between (Bound.Bound 0) (Bound.Bound upperBound) predicates

in  atMost
