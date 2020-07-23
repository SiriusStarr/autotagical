{-
# Input.Separator.keyword

## Function

Given nonempty `Text`, create a separator that matches that literal text.  For
example, `Input.Separator.keyword "["` will match the literal character `[`,
and `Input.Separator.keyword "|betweentags|"` would parse
`tag1|betweentags|tag2` as `[tag1, tag2]`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.Separator.keyword
```

-}

let Separator =
        ../../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../../Internal/InputFormat/Separator

in  Separator.Keyword : Text → Separator
