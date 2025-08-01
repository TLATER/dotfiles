{
  pkgs,
  flake-inputs,
  lib,
  ...
}:
{
  imports = [ ../../home-modules/way-displays.nix ];

  options._dotfiles = lib.mkOption {
    type = lib.types.str;
    default = "${flake-inputs.self}/home-config/dotfiles";
    description = "Path to the dotfiles in this repository";
  };
  config.home = {
    stateVersion = "20.09";

    activation.expireOldGenerations = lib.hm.dag.entryAfter [
      "writeBoundary"
    ] (pkgs.writers.writeNu "clean-generations.nu" (builtins.readFile ./clean-generations.nu)).outPath;
  };
}
