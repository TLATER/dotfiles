{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (flake-inputs.self.packages.${pkgs.system}) pass-rofi;
in {
  config = lib.mkIf config.custom.desktop-environment {
    home.packages = [pass-rofi];

    programs.rofi = {
      enable = true;
      font = "mono 11";
      terminal = "${pkgs.alacritty}";
      extraConfig = {
        modi = "drun";
        drun-display-format = "{name}";
        show-icons = true;
      };
    };
  };
}
