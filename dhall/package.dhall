let Config =
        ./Config/package.dhall sha256:2791d52bcdb69e85a4e433ce090ffe698f72f1a8ca1c687085edebccb77d1888
      ? ./Config/package.dhall

in    Config.config
    ∧ { Config
      , GlobPatterns =
            ./GlobPatterns/package.dhall sha256:116e3c55bcf84e892027a75f013cefeba054bc40984024f041c50fabc171d0b6
          ? ./GlobPatterns/package.dhall
      , InputFormat =
            ./InputFormat/package.dhall sha256:60b602b4b404262db028a8d86ce1f74c464add8cf7f8e7e38e91ed4155ccf607
          ? ./InputFormat/package.dhall
      , OutputFormat =
            ./OutputFormat/package.dhall sha256:dd8a8c715c9997c77e1b34cb829973d1aa45bac324d3678446c72baa47152a34
          ? ./OutputFormat/package.dhall
      , Predicate =
            ./Predicate/package.dhall sha256:23dce0b3922c90eb5068a18e14a8b175999cd4e39506065267e9fdd1eb021d09
          ? ./Predicate/package.dhall
      , RenamingSchema =
            ./RenamingSchema/package.dhall sha256:c9fd3b1f24d93a1cd2c53965487523234f06ec4fa6f4129f73b45bdbdb89030f
          ? ./RenamingSchema/package.dhall
      , SortingSchema =
            ./SortingSchema/package.dhall sha256:99678361c9270e3b409ec54dd0350e01de6f2a8e2a9a5b5972a213a17092aee9
          ? ./SortingSchema/package.dhall
      , Tag =
            ./Tag/package.dhall sha256:da707da6758d7d20006abc4f461628ad993bc42b5d53d30e183cfde17a991c59
          ? ./Tag/package.dhall
      , TagGroup =
            ./TagGroup/package.dhall sha256:49200ceb584e2ce838145bb57af5f0fbecc20ed1ad692a294dd0bca66e69ee09
          ? ./TagGroup/package.dhall
      }
