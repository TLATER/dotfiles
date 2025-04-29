{
  lib,
  writers,

  git,
  nushell,
  nvfetcher,
}:
writers.writeNuBin "commit-nvfetcher" {
  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    "${lib.makeBinPath [
      git
      nushell
      nvfetcher
    ]}"
  ];
} ./commit-nvfetcher.nu
