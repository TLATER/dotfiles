{
  pkgs,
  self,
}: let
  inherit (pkgs.lib) callPackageWith;
  mkTest = test:
    pkgs.stdenv.mkDerivation ({
        dontPatch = true;
        dontConfigure = true;
        dontBuild = true;
        dontInstall = true;
        doCheck = true;
      }
      // test);
  callPackage = callPackageWith (pkgs
    // {
      inherit mkTest;
      # Work around `self` technically being a store path when
      # evaluated as a flake - `builtins.filter` can otherwise not be
      # called on it.
      self = builtins.path {
        name = "dotfiles";
        path = self;
      };
    });
in {
  # Linters and formatters
  alejandra = callPackage ./alejandra.nix {};
  emacs = callPackage ./emacs.nix {};
  shellcheck = callPackage ./shellcheck.nix {};

  # Ensure that gcs actually builds
  gcs = pkgs.local.gcs;
}
