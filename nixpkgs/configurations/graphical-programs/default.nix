{ pkgs, dotroot, ... }:

{
  imports = [ ./dunst.nix ./mime.nix ./firefox.nix ./stumpwm.nix ];

  home.packages = with pkgs; [
    alacritty
    barrier
    feh
    llpp
    rofi
    xsel

    local.cap
    local.pass-rofi
  ];

  xdg.configFile = {
    "alacritty/alacritty.yml".source =
      "${dotroot}/dotfiles/alacritty/alacritty.yml";
  };

  services.caffeine.enable = true;
}
