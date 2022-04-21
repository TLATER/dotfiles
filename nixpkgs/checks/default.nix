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
  callPackage = callPackageWith (pkgs // {inherit self mkTest;});
in {
  # Linters and formatters
  alejandra = callPackage ./alejandra.nix {};
  emacs = callPackage ./emacs.nix {};
  shellcheck = callPackage ./shellcheck.nix {};

  # Ensure that gcs actually builds
  gcs = pkgs.local.gcs;
}
