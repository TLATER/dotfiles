{
  pkgs,
  dotroot,
  ...
}: {
  imports = [./aria.nix ./dunst.nix ./firefox.nix ./mime.nix];

  home.packages = with pkgs; [
    alacritty
    feh
    llpp
    rofi
    xsel

    local.cap
  ];

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

  xdg.configFile = {
    "alacritty/alacritty.yml".source = "${dotroot}/dotfiles/alacritty/alacritty.yml";
  };
}
