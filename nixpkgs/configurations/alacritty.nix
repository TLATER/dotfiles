{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
  ];

  xdg.configFile = {
    "alacritty/alacritty.yml".source = ../../dotfiles/alacritty/alacritty.yml;
  };
}
