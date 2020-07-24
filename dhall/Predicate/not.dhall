{-|
# Predicate.not

## Predicate

Given a predicate, negates its value, i.e. if the predicate evaluates to true,
it is false, while if the predicate is false, it is true.

## Examples

Tags: `tag1`, `tag2`, `tag3`

* `not (hasTag tag1): False`

* `not (always): False`

* `not (hasTag tag4): True`

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Predicate.not Autotagical.Predicate.always
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

let not
    : Predicate → Predicate
    = λ(p : Predicate) →
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

        in  λ(Fix : PredicateF → Predicate) →
              Fix (PredicateF.NotF (p Predicate Fix))

in  not
