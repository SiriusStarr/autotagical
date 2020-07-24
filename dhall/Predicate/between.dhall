{-|
# Predicate.between

## Predicate

Given a natural number `X`, a natural number `Y`, and a nonempty list of
predicates, evaluates to true if no fewer than `X` and no more than `Y` of the predicates evaluate to true; otherwise, evaluates to false.

## Examples

Tags: `tag1`, `tag2`, `tag3`

* `between 1 2 [hasTag "tag1", hasTag "tag2", hasTag "tag3"]: False`

* `between 1 2 [hasTag "tag1", hasTag "tag2", hasTag "tag4"]: True`

* `between 1 2 [hasTag "tag1", hasTag "tag4"]: True`

* `between 1 2 [hasTag "tag4"]: False`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Predicate.between
      1
      2
      [ Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag1")
      , Autotagical.Predicate.hasTag (Autotagical.Tags.tag "tag2")
      ]
```

-}

let internalBetween =
        ../Internal/Predicate/between sha256:d9e26bcb027406f03cb1b0cefc0faadbaea36ddf516d6f0ed6bad7476ae27129
      ? ../Internal/Predicate/between

let Bound =
        ../Internal/Predicate/Bound sha256:5df84f4de5c95a2e6e0cadea46dd4fcf4a0345e2190fe034f064351ede3ec30d
      ? ../Internal/Predicate/Bound

let Predicate =
        ../Internal/Predicate/Type sha256:7bbd5275d9b92c0c5b1c7c0f05885b85796a14c4813d02aae7148a9d418deedf
      ? ../Internal/Predicate/Type

let between
    : Natural → Natural → List Predicate → Predicate
    = λ(lowerBound : Natural) →
      λ(upperBound : Natural) →
      λ(predicates : List Predicate) →
        internalBetween
          (Bound.Bound lowerBound)
          (Bound.Bound upperBound)
          predicates

in  between
