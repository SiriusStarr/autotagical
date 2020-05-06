{ LeadingTagSeparator =
      ./LeadingTagSeparator/package.dhall sha256:cc6759c73367bdcc2fcebb6ca04de893e9bd0c3bcb4c712cd3b9c49e0d93352d
    ? ./LeadingTagSeparator/package.dhall
, TrailingTagSeparator =
      ./TrailingTagSeparator/package.dhall sha256:e23870ece3a0cca25ecbed331b8cdd22fdf6ee0f63aa611244c20218857c3152
    ? ./TrailingTagSeparator/package.dhall
, taggedValues =
      ./taggedValues sha256:de95a9d581495af5c04b1e578f5966249676182eaf6320c19c5a97d5d3e25451
    ? ./taggedValues
, tagsBeforeFileName =
      ./tagsBeforeFileName sha256:b5ba125726697577ed1d3be13436d4902a9b7b1fec3e4636ed09760a20b2d728
    ? ./tagsBeforeFileName
}
