{ fromText =
      ./fromText.dhall sha256:0d95cdd4b2891f647d7dadb03b2fda726aaec5a5d94ac6540a4ad6d22a5ebf8a
    ? ./fromText.dhall
, leafFromText =
      ./leafFromText.dhall sha256:c1ed5baf24996bc75ffad3f757f0b2073b630a8bfeacb7961b78751ac8995361
    ? ./leafFromText.dhall
, leafFromTemplate =
      ./leafFromTemplate.dhall sha256:9f2ca8de74502f42acdf0366dbf8506df1119be3a44df3b21e77fba4812d5ce3
    ? ./leafFromTemplate.dhall
, Name =
      ./Name/package.dhall sha256:7c26ba210b373431d418a79c98016be79d244070b5e1bc7ccd66cd7c997579e0
    ? ./Name/package.dhall
, fromTemplate =
      ./fromTemplate.dhall sha256:b38c725c90a8e91a6fa10c3334271ead9a8dd153840cb3a026eb0880c6ae3b0d
    ? ./fromTemplate.dhall
}
