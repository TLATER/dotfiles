{ pkgs, dotroot, ... }:

{
  imports = [ ./dunst.nix ./firefox.nix ./mime.nix ];

  home.packages = with pkgs; [
    alacritty
    feh
    llpp
    rofi
    xsel

    local.cap
  ];

  xdg.configFile = {
    "alacritty/alacritty.yml".source =
      "${dotroot}/dotfiles/alacritty/alacritty.yml";
  };
}
