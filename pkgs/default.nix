{ pkgs, flake-inputs }:
let
  localLib = import ./lib.nix { inherit pkgs flake-inputs; };

  # TODO(tlater): ast-grep supports nix starting with version 0.39
  inherit (flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}) ast-grep;
in
pkgs.lib.packagesFromDirectoryRecursive {
  callPackage = pkgs.lib.callPackageWith (pkgs // { inherit flake-inputs localLib ast-grep; });
  directory = ./packages;
}
