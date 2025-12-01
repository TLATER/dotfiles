{ pkgs, flake-inputs }:
let
  localLib = import ./lib.nix { inherit pkgs flake-inputs; };
in
pkgs.lib.packagesFromDirectoryRecursive {
  callPackage = pkgs.lib.callPackageWith (pkgs // { inherit flake-inputs localLib; });
  directory = ./packages;
}
