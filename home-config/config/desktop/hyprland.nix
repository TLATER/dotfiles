{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf config.custom.desktop-environment {
    home.packages = with pkgs; [
      slurp
      grim
    ];

    wayland.windowManager.hyprland = {
      enable = true;
      systemdIntegration = true;
      nvidiaPatches = true;

      extraConfig = builtins.readFile ../../dotfiles/hyprland.conf;
    };
  };
}
