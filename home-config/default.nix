{
  imports = [
    ../home-modules/way-displays.nix
    ../home-modules/clean-generations
  ];

  config.home = {
    cleanGenerations.enable = true;
    stateVersion = "20.09";
  };
}
