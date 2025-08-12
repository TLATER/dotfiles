{ lib, ... }:
{
  imports = [
    ../home-modules/way-displays.nix
    ../home-modules/clean-generations
  ];

  options.allowThirdPartyPackages = lib.mkEnableOption "allow non-nixpkgs packages";

  config.home = {
    cleanGenerations.enable = true;
    stateVersion = "20.09";
  };
}
