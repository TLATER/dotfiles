{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./firefox.nix
    ./discord.nix
    ./element.nix
  ];

  config = lib.mkIf config.custom.graphical-applications {
    home.packages = with pkgs; [
      alacritty
      feh
      llpp
      xsel
      yubioath-desktop
    ];

    xdg.configFile."alacritty/alacritty.yml".source = "${config._dotfiles}/alacritty/alacritty.yml";
  };
}
