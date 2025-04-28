{
  self,
  flake-inputs,
  system,
  lib,
  mkTest,

  deadnix,
  fd,
  shellcheck,
  statix,
}:
let
  inherit (flake-inputs.nixpkgs-unstable.legacyPackages.${system}) nixfmt-rfc-style;
in
mkTest {
  name = "lints";
  src = lib.cleanSourceWith { src = self; };

  checkInputs = [
    fd

    deadnix
    nixfmt-rfc-style
    shellcheck
    statix
  ];

  checkPhase = builtins.readFile ./lints.sh;
}
