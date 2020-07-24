{-|
# Input.Separator.whitespace

## Value

A separator that matches **one or more** whitespace characters.  If you need to
match a **specific** number of a **specific** character, just use
`Input.Separator.keyword`.

## Usage

```dhall
let Autotagical = https://siriusstarr.github.io/autotagical/dhall/package.dhall

in  Autotagical.Input.Separator.whitespace
```

-}

let Separator =
        ../../Internal/InputFormat/Separator sha256:0e5d8464af2df9c7ffae603f39803634d3dcecb28de50af26875820a1074c36d
      ? ../../Internal/InputFormat/Separator

in  Separator.Whitespace : Separator
