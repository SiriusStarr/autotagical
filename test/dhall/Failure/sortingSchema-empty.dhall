let S = ../../../dhall/SortingSchema/package.dhall

let Folder = ../../../dhall/Internal/SortingSchema/Folder

in  S.sortingSchema ([] : List Folder)
