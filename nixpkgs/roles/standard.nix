{ config, lib, pkgs, ... }:

with lib;

let
  helpers = import ../helpers { inherit lib; };
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  options = {
    isWorkProfile = mkOption {
      type = types.bool;
      default = false;
    };

    screenWidth = mkOption {
      type = types.int;
      default = 1920;
    };
  };

  imports = [
    ../configurations/alacritty.nix
    ../configurations/dunst.nix
    ../configurations/emacs.nix
    ../configurations/graphical-programs.nix
    ../configurations/mail.nix
    ../configurations/misc.nix
    ../configurations/stumpwm.nix
    ../configurations/tty-programs.nix
    ../configurations/zsh.nix
  ];

  config = {
    home.packages = with pkgs; [
      feh
      llpp
      rofi
      scrot
      xsel

      # Dev things
      gcc # Required for rustc (mozilla/nixpkgs-mozilla#22)
      rustup

      local-pkgs.pass-rofi
    ];

    programs.direnv.enable = true;
    programs.home-manager.enable = true;
    home.stateVersion = "19.09";
  };
}
