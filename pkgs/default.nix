{
  pkgs,
  flake-inputs,
}:
let
  sources = pkgs.callPackage ./sources.nix { };
in
pkgs.lib.packagesFromDirectoryRecursive {
  callPackage = pkgs.lib.callPackageWith (pkgs // { inherit sources flake-inputs; });
  directory = ./packages;
}
