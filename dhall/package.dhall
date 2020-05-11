let Config =
        ./Config/package.dhall sha256:9879d7bbdad64c8f00bd41e2e18979f77a506adeace141779649ad11d5eb7e43
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
            ./Predicate/package.dhall sha256:2b93a271f7409d961486dde301ef44b3f0e2d549b256e3f254eb6eba5863af70
          ? ./Predicate/package.dhall
      , RenamingSchema =
            ./RenamingSchema/package.dhall sha256:7fab0144aebad86b9da0f13efacb7632c23255acab661bd63984418ac596f26a
          ? ./RenamingSchema/package.dhall
      , SortingSchema =
            ./SortingSchema/package.dhall sha256:d12ef352abac8d038fc4fe3e1a26fc52c80eb02e5ad56c90e2b0255e2b8d2db5
          ? ./SortingSchema/package.dhall
      , Tag =
            ./Tag/package.dhall sha256:da707da6758d7d20006abc4f461628ad993bc42b5d53d30e183cfde17a991c59
          ? ./Tag/package.dhall
      , TagGroup =
            ./TagGroup/package.dhall sha256:c0c57d4995b05347690803964a36a22e359c6e6dd19b7e02d510f3e0c43ef21e
          ? ./TagGroup/package.dhall
      }
