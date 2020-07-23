{-
# Input.Separator.none

## Value

This indicates the **absence** of a separator.  It is only valid for pre- and
post-tagged value separators.  All other separators must exist.  Don't worry,
you can't accidentally use this in the wrong place, because it returns an
`Optional Separator`, which is a type error elsewhere.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.Separator.none
```

-}

let Separator =
        ../../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../../Internal/InputFormat/Separator

in  None Separator : Optional Separator
