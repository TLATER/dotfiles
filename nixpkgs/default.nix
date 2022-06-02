{
  lib,
  pkgs,
  ...
}: {
  _module.args.dotroot = ./..;

  imports = [./configurations/xdg-settings.nix];

  home.stateVersion = "20.09";
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
