{ config, pkgs, ... }:
{
  imports = [
    ./discord.nix
    ./element.nix
    ./firefox.nix
    ./keepassxc.nix
    ./whatsapp.nix
  ];

  home.packages = with pkgs; [
    alacritty
    apvlv
    feh
    xsel
    yubioath-flutter
  ];

  xdg.configFile."alacritty/alacritty.toml".source = "${config._dotfiles}/alacritty/alacritty.toml";
}
