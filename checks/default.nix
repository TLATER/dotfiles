{ nixpkgs, ... }@flake-inputs:
let
  pkgs = nixpkgs.legacyPackages;

  mkTest =
    test:
    pkgs.stdenvNoCC.mkDerivation (
      {
        dontPatch = true;
        dontConfigure = true;
        dontBuild = true;
        dontInstall = true;
        dontFixup = true;
        doCheck = true;
      }
      // test
    );

  callPackage = pkgs.lib.callPackageWith (
    pkgs
    // {
      inherit flake-inputs mkTest;
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
pkgs.lib.packagesFromDirectoryRecursive {
  inherit callPackage;
  directory = ./checks;
}
