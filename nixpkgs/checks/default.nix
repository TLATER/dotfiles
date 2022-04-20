{
  pkgs,
  self,
}: let
  inherit (pkgs.lib) callPackageWith;
  callPackage = callPackageWith (pkgs // {inherit self;});
in {
  # Linters and formatters
  alejandra = callPackage ./alejandra.nix {};
  emacs = callPackage ./emacs.nix {};
  shellcheck = callPackage ./shellcheck.nix {};

  # Ensure that gcs actually builds
  gcs = pkgs.local.gcs;
}
