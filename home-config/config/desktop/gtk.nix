{ pkgs, ... }:
{
  gtk = {
    enable = true;

    font = {
      package = pkgs.noto-fonts;
      name = "NotoSans";
    };

    theme = {
      package = pkgs.magnetic-catppuccin-gtk;
      name = "Catppuccin-GTK-Dark";
    };

    cursorTheme = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Original-Ice";
      size = 24;
    };
  };
}
