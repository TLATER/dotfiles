{
  self,
  flake-inputs,
  system,
  lib,
  mkTest,
  fetchpatch,

  deadnix,
  nixfmt-rfc-style,
  nushell,
  shellcheck,
  statix,
}:
let
  statix' = statix.overrideAttrs (old: {
    patches = old.patches ++ [
      (fetchpatch {
        url = "https://github.com/oppiliappan/statix/commit/925dec39bb705acbbe77178b4d658fe1b752abbb.patch";
        hash = "sha256-0wacO6wuYJ4ufN9PGucRVJucFdFFNF+NoHYIrLXsCWs=";
      })
    ];
  });
in
mkTest {
  name = "lints";

  # We *could* define the regexes for the files to include in our
  # `src` here, which would make nix only run the checks if any
  # relevant files have been changed.
  #
  # The runtime is already super tiny, though, so skipping the build
  # is almost pointless, and it's not like we'll ever not touch a nix
  # file.
  #
  # *Maybe* if the repo grows an order of magnitude or two and we
  # split apart the nix and shell checks again.
  src = lib.cleanSourceWith {
    src = self;
    filter = lib.cleanSourceFilter;
  };

  checkInputs = [
    nushell

    deadnix
    nixfmt-rfc-style
    shellcheck
    statix'
    flake-inputs.self.packages.${system}.topiary
  ];

  checkPhase = ''
    nu ${./lints.nu}
  '';
}
