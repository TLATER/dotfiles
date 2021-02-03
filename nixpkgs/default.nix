{ lib, pkgs, ... }:

{
  _module.args.dotroot = ./..;

  home.stateVersion = "20.09";
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };
}
