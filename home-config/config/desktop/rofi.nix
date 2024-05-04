{ pkgs, ... }:
{
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
}
