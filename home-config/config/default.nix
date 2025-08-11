{ flake-inputs, lib, ... }:
{
  imports = [
    ../../home-modules/way-displays.nix
    ../../home-modules/clean-generations
  ];

  options._dotfiles = lib.mkOption {
    type = lib.types.str;
    default = "${flake-inputs.self}/home-config/dotfiles";
    description = "Path to the dotfiles in this repository";
  };

  config.home = {
    cleanGenerations.enable = true;
    stateVersion = "20.09";
  };
}
