{
  self,
  flake-inputs,
  system,
  lib,
  mkTest,

  deadnix,
  fd,
  nushell,
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
    nushell

    deadnix
    nixfmt-rfc-style
    shellcheck
    statix
  ];

  checkPhase = ''
    nu ${./lints.nu}
  '';
}
