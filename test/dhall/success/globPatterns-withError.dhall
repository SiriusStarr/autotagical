let P = ../../../dhall/Glob/package.dhall

in  P.patterns [ "*.jpg", "*[/*.png", "literal" ] ⫽ P.With.errorRecovery
