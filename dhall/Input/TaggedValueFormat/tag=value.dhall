{-
# Input.TaggedValueFormat.tag=value

## Function

Given a boolean indicating whether pure tags (i.e. non-tagged values) are
allowed, returns a tagged value format that recognizes the standard `tag=value`
format for tagged values.

Note that since this uses an `=` in the record field, it must be accessed by
surrounding the field with backticks (\`), as shown in Usage below.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.TaggedValueFormat.`tag=value` True
```

-}

let Separator =
        ../../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../../Internal/InputFormat/Separator

let TaggedValueFormat =
        ../../Internal/InputFormat/TaggedValueFormat sha256:62b80103bf88b5ca5f35005b578267a5679fdbdcfb0b77229c3651b882961551
      ? ../../Internal/InputFormat/TaggedValueFormat

let tagsBeforeValues =
        ./tagsBeforeValues.dhall sha256:6eefebd6d8d480f918016e8a69060e0b826f3fee336cdcc18e3e655d6f95f18d
      ? ./tagsBeforeValues.dhall

let `tag=value`
    : Bool → TaggedValueFormat
    = λ(pureTagsAllowed : Bool) →
        tagsBeforeValues
          (None Separator)
          (Separator.Keyword "=")
          (None Separator)
          pureTagsAllowed

in  `tag=value`
