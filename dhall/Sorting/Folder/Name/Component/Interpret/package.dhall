{ numberAsMonth =
      ./numberAsMonth.dhall sha256:b4d923a161c5bd5f77dbf68f5dfb007a0528db5fe82919e8511fa6efa693b53d
    ? ./numberAsMonth.dhall
, numberAsOrdinal =
      ./numberAsOrdinal.dhall sha256:0ff9824d4c85c3fc310095f8069c93c3084d24fdc5e84f7968822772bdf38acf
    ? ./numberAsOrdinal.dhall
, numberAsYear =
      ./numberAsYear.dhall sha256:7902fa3e098d40053798121f86a98fd9121150ba7c687d78e6545bcfffeb6cd6
    ? ./numberAsYear.dhall
, MonthFormat =
      ./MonthFormat.dhall sha256:dfd6b189f8bb549ba658f2920c2e586c4f47842badfae531a21b215882b773b2
    ? ./MonthFormat.dhall
, YearFormat =
      ./YearFormat.dhall sha256:d19143b5ab758fc6d480f319dad48a2ad2a6d28760ae5efa0de86162d927f472
    ? ./YearFormat.dhall
}
