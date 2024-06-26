{
  pkgs,
  lib,
  flake-inputs,
}:
let
  inherit (lib) callPackageWith;

  generatedFiles = [
    "home-config/dotfiles/emacs.d/share/templates"
    "pkgs/_sources"
    "nixos-config/hosts/yui/hardware-configuration.nix"
    "nixos-config/hosts/ren/hardware-configuration.nix"
    "nixos-config/hosts/rin/hardware-configuration.nix"
  ];

  mkTest =
    test:
    pkgs.stdenv.mkDerivation (
      {
        dontPatch = true;
        dontConfigure = true;
        dontBuild = true;
        dontInstall = true;
        doCheck = true;
      }
      // test
    );

  callPackage = callPackageWith (
    pkgs
    // {
      inherit flake-inputs mkTest generatedFiles;
      # Work around `self` technically being a store path when
      # evaluated as a flake - `builtins.filter` can otherwise not be
      # called on it.
      self = builtins.path {
        name = "dotfiles";
        path = flake-inputs.self;
      };
    }
  );
in
{
  # Linters and formatters
  deadnix = callPackage ./deadnix.nix { };
  nixfmt = callPackage ./nixfmt.nix { };
  shellcheck = callPackage ./shellcheck.nix { };
  statix = callPackage ./statix.nix { };
}
