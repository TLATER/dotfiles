{ pkgs, dotroot, ... }:

{
  imports = [ ./dunst.nix ./mime.nix ./firefox.nix ./stumpwm.nix ];

  home.packages = with pkgs; [
    alacritty
    feh
    llpp
    rofi
    scrot
    xsel

    local.cap
    local.pass-rofi
  ];

  # services.caffeine.enable = true;
  xdg.configFile = {
    "alacritty/alacritty.yml".source =
      "${dotroot}/dotfiles/alacritty/alacritty.yml";
  };
}
