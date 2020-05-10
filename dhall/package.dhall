let Config =
        ./Config/package.dhall sha256:908c16961eeed3a83c4c74896641cb1f54cadf00eeef3e18f3cb311fb1085410
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
            ./Predicate/package.dhall sha256:7f7dc99bbe90e97fded9d4b1b34adf51f01e6c9491237c15dfb65726671fa906
          ? ./Predicate/package.dhall
      , RenamingSchema =
            ./RenamingSchema/package.dhall sha256:2db6383463ecf80c41bd43e7d7f059318b11652acf3cfa7f3cff1fd27c111a58
          ? ./RenamingSchema/package.dhall
      , SortingSchema =
            ./SortingSchema/package.dhall sha256:63b365a65fa465055f8b0482a46b690449964d9ffaecf693a2c8b05f62fa725b
          ? ./SortingSchema/package.dhall
      , Tag =
            ./Tag/package.dhall sha256:da707da6758d7d20006abc4f461628ad993bc42b5d53d30e183cfde17a991c59
          ? ./Tag/package.dhall
      , TagGroup =
            ./TagGroup/package.dhall sha256:49200ceb584e2ce838145bb57af5f0fbecc20ed1ad692a294dd0bca66e69ee09
          ? ./TagGroup/package.dhall
      }
